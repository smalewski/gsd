{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TupleSections #-}
module Interpreter.Eval where

import Control.Monad.Except (ExceptT)
import Data.List (find)
import Interpreter.Env
import Interpreter.Error (ErrorInfo(..), ErrorLevel (Error))
import Interpreter.Syntax.Common
import Interpreter.Syntax.EvCore
import Interpreter.Type
import qualified Control.Monad.Writer as W (tell)
import Data.IORef
import Data.Map.Strict (fromList)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))
import Data.Aeson (decodeStrict', encode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text)
import Data.ByteString.Char8 as B (concat)
import Data.ByteString.Lazy.Char8 (toChunks)

type State = (Expr, Env Value, Kont)

data Kont
  = KEmpty
  | KAsc Evidence Type (Env Value) Kont
  | KApp Expr (Env Value) Kont
  | KFun Expr Kont
  | KAccess LabelName Type (Env Value) Kont
  | KCtor CtorName [CtorArgValue] LabelName [CtorArg] (Env Value) Kont
  | KMatch [Case] (Env Value) Kont
  | KBinOpL BinOp Expr Kont
  | KBinOpR BinOp Expr Kont
  deriving (Show)

data Error
  = EMatch CtorName
  | EAccess CtorName LabelName
  | ETrans Type Type
  | EVar Name
  | EToJSON Expr
  | EFromJSON Text

instance ErrorInfo Error where
  errorLvl _ = Error
  errorTitle _ = "Runtime error"

type CtorArgValue = CtorArg

type EvalM = EnvM IO Value [Expr] Error

eval :: Bool -> Env Value -> Expr -> IO (Either Error Expr, [Expr])
eval trace env = removeDupSteps . evalEnvMIO env . go 0 . inject env
  where
    go :: Int -> State -> EvalM Expr
    go n s@(e, _, k)
      | n > 1000 || isFinal s = tell [applyKont k e] $> applyKont k e
      | otherwise             = tell [highlight k e] *> step tell s >>= go (n + 1)
--      | otherwise             = step tell s >>= go (n + 1)

    removeDupSteps  = (fmap . fmap) removeDupSteps'
    removeDupSteps' (x:y:xs)
      | x == y    = x : removeDupSteps' xs
      | otherwise = x : removeDupSteps' (y : xs)
    removeDupSteps' xs = xs

    tell x = if trace then W.tell x else pure ()

initValEnv :: Bool -> Env Type -> [(Name, Expr)] -> [(Name, Expr)] -> ExceptT Error IO (Env Value)
initValEnv trace env funs konst = do
  -- Create dummy environment
  refFuns   <- liftIO $ mapM (\(x,e) -> (x, ) . Boxed <$> newIORef e) funs
  refKonst  <- liftIO $ mapM (\(x,e) -> (x, ) . Boxed <$> newIORef e) konst
  let valEnv   = Env (dataCtx env) (ctorCtx env) (fromList $ refFuns <> refKonst)

  -- Evaluate
  liftIO $ mapM_ (cicleEval trace valEnv) refFuns
  liftIO $ mapM_ (cicleEval trace valEnv) refKonst

  pure valEnv

cicleEval :: Bool -> Env Value -> (Name, Value) -> IO (Either Error (), [Expr])
cicleEval _ _ (_, Unboxed _) = pure (Right (), [])
cicleEval trace env (_, Boxed box) = do
  v  <- readIORef box
  (eith, acc) <- eval trace env v
  case eith of
    Left err'     -> pure (Left err', acc)
    Right v' -> writeIORef box v' $> (Right (), acc)

highlight :: Kont -> Expr -> Expr
highlight k = applyKont k . Highlight

applyKont :: Kont -> Expr -> Expr
applyKont KEmpty e = e
applyKont (KAsc ev t _ k) e = applyKont k (Asc ev e t)
applyKont (KApp e2 _ k) e = applyKont k (App e e2)
applyKont (KFun e1 k) e = applyKont k (App e1 e)
applyKont (KAccess l t _ k) e = applyKont k (Access e l t)
applyKont (KCtor c vs l es _ k) e =
  let args = vs <> [CtorArg l e] <> es
   in applyKont k (Ctor c args)
applyKont (KMatch cs _ k) e = applyKont k (Match e cs)
applyKont (KBinOpL bop e2 k) e = applyKont k (BinOp bop e e2)
applyKont (KBinOpR bop e1 k) e = applyKont k (BinOp bop e1 e)

inject :: Env Value -> Expr -> State
inject env e = (e, env, KEmpty)

isFinal :: State -> Bool
isFinal (e, _, KEmpty) = isValue e || isRawValue e
isFinal _ = False

step :: ([Expr] -> EvalM ()) -> State -> EvalM State
-- reduce

-- FromJSON
step tell (v0@(Value ev2 (Lit (LString txt)) _), env, k0@(KFun (Value ev1 FromJson (TArr _ _ t12)) k)) =
  do
    let Evidence (TArr _ ev11 ev12) = ev1
        ev = Evidence (TUnknData mempty)
    _  <- trans ev2 (Evidence ev11)
    ev' <- trans ev (Evidence ev12)
    case decodeStrict' $ encodeUtf8 txt of
      Nothing -> err $ EFromJSON txt
      Just json -> do
        let v = Value ev' json t12
--        tell [highlight k0 v0, highlight k v]
        pure (v, env, k)

-- ToJSON
step tell (v0@(Value ev2 u _), env, k0@(KFun (Value ev1 ToJson (TArr _ _ t12)) k)) =
  do
    let Evidence (TArr _ ev11 ev12) = ev1
        ev = Evidence (TBase mempty TString)
    _   <- trans ev2 (Evidence ev11)
    ev' <- trans ev (Evidence ev12)
    let txt = decodeUtf8 . B.concat . toChunks $ encode u
        v = Value ev' (Lit $ LString txt) t12
--    tell [highlight k0 v0, highlight k v]
    pure (v, env, k)

-- R-Beta
step tell (v0@(Value ev2 u _), _, k0@(KFun (Value ev1 (Clos x tx e env) (TArr _ _ t12)) k)) =
  do
    let Evidence (TArr _ ev11 ev12) = ev1
    ev <- trans ev2 (Evidence ev11)
    let v      = Value ev u tx
        newEnv = insertVar env x (Unboxed v)
        v'     = Asc (Evidence ev12) e t12
    --tell [highlight k0 v0, highlight k v']
    pure (v', newEnv, k)

-- R-Delta
step tell (v0@(Value _ u2 _), env, k0@(KBinOpR bop (Value _ u1 _) k)) =
  do
    let (_, _, t) = binOpType bop
        ev = Evidence t
        u = evalBinOp bop u1 u2
        v' = Value ev u t
    --tell [highlight k0 v0, highlight k v']
    pure (v', env, k)

-- R-AscErase
step tell (v0@(Value ev1 u _), _, k0@(KAsc ev2 t2 env k)) =
    do
      ev <- trans ev1 ev2
      let v' = Value ev u t2
      --tell [highlight k0 v0, highlight k v']
      pure (v', env, k)

-- R-Match
step tell (v0@(Value _ (Ctor c args) _), _, k0@(KMatch cs env k)) =
    case find (matches c (length args) . casePattern) cs of
      Nothing -> err $ EMatch c
      Just (Case p e) ->
        let xvs = zip (pvar p) (Unboxed . ctorArgExpr <$> args)
            newEnv = extendVarCtx env xvs
--        in  tell [highlight k0 v0, highlight k e] $> (e, newEnv, k)
        in  pure (e, newEnv, k)

-- R-Access
step tell (v0@(Value _ (Ctor c args) _), _, k0@(KAccess l t' env k)) =
    case find ((l ==) . ctorArgLabel) args of
      Just (CtorArg _ (Value evk uk _)) -> do
        ev' <- trans evk (Evidence t')
        let v' = Value ev' uk t'
        --tell [highlight k0 v0, highlight k v']
        pure (v', env, k)
      _ -> err $ EAccess c l

-- To value

step _ (Lam x tx e, env, k)
  = pure (Clos x tx e env, env, k)

step _ (u, _, KAsc ev t env k)
  | isRawValue u = pure (Value ev u t, env, k)

-- Reduce

-- Intros

step tell (Var x@(Name _ name), env, k) = do
  v <- case lookupVar' x env of
    Just (Boxed box) -> liftIO $ readIORef box
    Just (Unboxed v) -> pure v
    Nothing
      | name == "fromJSON" -> pure FromJson
      | name == "toJSON"   -> pure ToJson
      | otherwise          -> err $ EVar x
  --tell [highlight k (Var x), highlight k v]
  pure (v, env, k)

step _ (App e1 e2, env, k) =
  pure (e1, env, KApp e2 env k)
step _ (Asc ev e t, env, k) =
  pure (e, env, KAsc ev t env k)
step _ (Ctor c (CtorArg l e : args), env, k) =
  pure (e, env, KCtor c [] l args env k)
step _ (Match e cs, env, k) =
  pure (e, env, KMatch cs env k)
step _ (BinOp bop e1 e2, env, k) =
  pure (e1, env, KBinOpL bop e2 k)
step _ (Access e l t, env,  k) =
  pure (e, env, KAccess l t env k)

-- Elimination

step _ (v, _, KApp e2 env k)
  | isValue v =
    pure (e2, env, KFun v k)
step _ (v, env, KBinOpL bop e2 k) =
  pure (e2, env, KBinOpR bop v k)
step _ (v, _, KCtor c vs l (CtorArg l' e : cs) env k) =
  pure (e, env, KCtor c (CtorArg l v : vs) l' cs env k)
step _ (v, _, KCtor c vs l [] env k) =
  pure (Ctor c (reverse (CtorArg l v : vs)), env, k)

-- Stuck?
step _ (x, env, _) = pure (x, env, KEmpty)

matches :: CtorName -> Int -> Pattern -> Bool
matches c n (CtorP _ c' args) = c == c' && n == length args
matches _ _ (DefP _) = True

evalBinOp :: BinOp -> Expr -> Expr -> Expr
evalBinOp Plus (Lit (LInt n1)) (Lit (LInt n2)) = Lit (LInt $ n1 + n2)
evalBinOp Minus (Lit (LInt n1)) (Lit (LInt n2)) = Lit (LInt $ n1 - n2)
evalBinOp Times (Lit (LInt n1)) (Lit (LInt n2)) = Lit (LInt $ n1 * n2)
evalBinOp Div (Lit (LInt n1)) (Lit (LInt n2)) = Lit (LInt $ n1 `div` n2)
evalBinOp Equal u1 u2 = if u1 == u2 then ctorTrue else ctorFalse
evalBinOp _ _ _ = Lit (LInt (-1))

trans :: Evidence -> Evidence -> EvalM Evidence
trans (Evidence t1) (Evidence t2) =
   maybe (err $ ETrans t1 t2) (pure . Evidence) (meet t1 t2)
