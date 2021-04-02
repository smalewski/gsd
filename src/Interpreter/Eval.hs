{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TupleSections #-}
module Interpreter.Eval where

import Control.Monad.Except (runExceptT, ExceptT)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (asks, runReaderT)
import Data.List (find)
import Interpreter.Auxiliary
import Interpreter.Env
import Interpreter.Error (ErrorTxt(errorTxt))
import Interpreter.Syntax.Common
import Interpreter.Syntax.EvCore
import Interpreter.Type
import Control.Monad.Writer (tell, WriterT (runWriterT))
import Interpreter.Printer (ppr)
import Debug.Trace (trace)
import Data.IORef
import Data.Map.Strict (fromList)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))

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

instance ErrorTxt Error where
  errorTxt (EMatch c)
    = (Nothing, rErr,
       "No case matches constructor $" <> ppr c
         <> "$ in match expression.")
  errorTxt (EAccess c l)
    = (Nothing, rErr, "Constructor $" <> ppr c <> "$ doesn't have label $" <> ppr l <> "$.")
  errorTxt (ETrans t1 t2)
    = (Nothing, rErr, "Consistent transitivity between $" <> ppr t1 <> "$ and $" <> ppr t2 <> "$ is not defined.")
  errorTxt (EVar n)
    = (Nothing, rErr, "Variable $" <> ppr n <> "$ is not in scope.")

rErr = "Runtime Error"

type CtorArgValue = CtorArg

type EvalM = EnvM IO Value [Expr] Error

eval :: Env Value -> Expr -> IO (Either Error (Expr, [Expr]))
eval env = removeDupSteps . evalEnvMIO env . go 0 . inject env
  where
    go :: Int -> State -> EvalM Expr
    go n s@(e, env, k)
      | n > 1000  || isFinal s = tell [applyKont k e] $> applyKont k e
      | otherwise             = step s >>= go (n + 1)

    removeDupSteps  = (fmap . fmap . fmap) removeDupSteps'
    removeDupSteps' (x:y:xs)
      | x == y    = x : removeDupSteps' xs
      | otherwise = x : removeDupSteps' (y : xs)
    removeDupSteps' xs = xs

initValEnv :: Env Type -> [(Name, Expr)] -> [(Name, Expr)] -> ExceptT Error IO (Env Value)
initValEnv env funs konst = do
  -- Create dummy environment
  refFuns   <- liftIO $ mapM (\(x,e) -> (x, ) . Boxed <$> newIORef e) funs
  refKonst  <- liftIO $ mapM (\(x,e) -> (x, ) . Boxed <$> newIORef e) konst
  let valEnv   = Env (dataCtx env) (ctorCtx env) (fromList $ refFuns <> refKonst)

  -- Evaluate
  liftIO $ mapM_ (cicleEval valEnv) refFuns
  liftIO $ mapM_ (cicleEval valEnv) refKonst

  pure valEnv

cicleEval :: Env Value -> (Name, Value) -> IO (Either Error ())
cicleEval env (x, Unboxed e) = pure $ pure ()
cicleEval env (x, Boxed box)   = do
  v  <- readIORef box
  eith <- eval env v
  case eith of
    Left err -> pure $ Left err
    Right (v', _) -> pure <$> writeIORef box v'

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

step :: State -> EvalM State
-- reduce

-- R-Beta
step (v0@(Value ev2 u t2), _, k0@(KFun (Value ev1 (Clos x tx e env) (TArr _ t11 t12)) k)) =
  do
    let Evidence (TArr _ ev11 ev12) = ev1
    ev <- trans ev2 (Evidence ev11)
    let v      = Value ev u tx
        newEnv = insertVar env x (Unboxed v)
        v'     = Asc (Evidence ev12) e t2
    tell [highlight k0 v0, highlight k v']
    pure (v', newEnv, k)

-- R-Delta
step (v0@(Value ev2 u2 t2), env, k0@(KBinOpR bop (Value ev1 u1 t1) k)) =
  do
    let (_, _, t) = binOpType bop t1
        ev = Evidence t
        u = evalBinOp bop u1 u2
        v' = Value ev u t
    tell [highlight k0 v0, highlight k v']
    pure (v', env, k)

-- R-AscErase
step (v0@(Value ev1 u t1), _, k0@(KAsc ev2 t2 env k)) =
    do
      ev <- trans ev1 ev2
      let v' = Value ev u t2
      tell [highlight k0 v0, highlight k v']
      pure (v', env, k)

-- R-Match
step (v0@(Value ev u@(Ctor c args) t), _, k0@(KMatch cs env k)) =
    case find (matches c . casePattern) cs of
      Nothing -> err $ EMatch c
      Just (Case p e) ->
        let xvs = zip (pvar p) (Unboxed . ctorArgExpr <$> args)
            newEnv = extendVarCtx env xvs
        in  tell [highlight k0 v0, highlight k e] *> pure (e, newEnv, k)

-- R-Access
step (v0@(Value ev (Ctor c args) t), _, k0@(KAccess l t' env k)) =
    case find ((l ==) . ctorArgLabel) args of
      Just (CtorArg _ (Value evk uk tk)) -> do
        ev' <- trans evk (Evidence t')
        let v' = Value ev' uk t'
        tell [highlight k0 v0, highlight k v']
        pure (v', env, k)
      _ -> err $ EAccess c l

-- To value

step (Lam x tx e, env, k)
  = pure (Clos x tx e env, env, k)

step (u, _, KAsc ev t env k)
  | isRawValue u = pure (Value ev u t, env, k)

-- Reduce

-- Intros

step (Var x, env, k) = do
  obj <- maybe (err $ EVar x) pure $ lookupVar' x env
  v' <- case obj of
          Boxed box -> liftIO $ readIORef box
          Unboxed v -> pure v
  tell [highlight k (Var x), highlight k v']
  pure (v', env, k)

step (App e1 e2, env, k) =
  pure (e1, env, KApp e2 env k)
step (Asc ev e t, env, k) =
  pure (e, env, KAsc ev t env k)
step (Ctor c (CtorArg l e : args), env, k) =
  pure (e, env, KCtor c [] l args env k)
step (Match e cs, env, k) =
  pure (e, env, KMatch cs env k)
step (BinOp bop e1 e2, env, k) =
  pure (e1, env, KBinOpL bop e2 k)
step (Access e l t, env,  k) =
  pure (e, env, KAccess l t env k)

-- Elimination

step (v, _, KApp e2 env k)
  | isValue v =
    pure (e2, env, KFun v k)
step (v, env, KBinOpL bop e2 k) =
  pure (e2, env, KBinOpR bop v k)
step (v, _, KCtor c vs l (CtorArg l' e : cs) env k) =
  pure (e, env, KCtor c (CtorArg l v : vs) l' cs env k)
step (v, _, KCtor c vs l [] env k) =
  pure (Ctor c (reverse (CtorArg l v : vs)), env, k)

-- Stuck?
step (x, env, _) = pure (trace "Stuck" x, env, KEmpty)

matches :: CtorName -> Pattern -> Bool
matches c (CtorP _ c' _) = c == c'
matches _ (DefP _) = True

evalBinOp :: BinOp -> Expr -> Expr -> Expr
evalBinOp Plus (Lit (LInt n1)) (Lit (LInt n2)) = Lit (LInt $ n1 + n2)
evalBinOp Minus (Lit (LInt n1)) (Lit (LInt n2)) = Lit (LInt $ n1 - n2)
evalBinOp Times (Lit (LInt n1)) (Lit (LInt n2)) = Lit (LInt $ n1 * n2)
evalBinOp Div (Lit (LInt n1)) (Lit (LInt n2)) = Lit (LInt $ n1 `div` n2)
evalBinOp Equal u1 u2 = if u1 == u2 then ctorTrue else ctorFalse
evalBinOp _ _ _ = Lit (LInt (-1))

trans :: Evidence -> Evidence -> EvalM Evidence
trans (Evidence t1) (Evidence t2) =
  meet t1 t2 >>= maybe (err $ ETrans t1 t2) (pure . Evidence)

ctorTrue :: Expr
ctorTrue = Ctor (CtorName mempty "True") []

ctorFalse :: Expr
ctorFalse = Ctor (CtorName mempty "False") []
