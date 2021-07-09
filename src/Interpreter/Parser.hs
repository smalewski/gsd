{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Parser where

import Prelude hiding (span)
import Data.Map.Strict (singleton, fromList)
import qualified Data.Map.Strict as Map
import Interpreter.Env
import Interpreter.Parser.Helpers hiding (ParseError)
import Interpreter.Syntax.Common
import Interpreter.Type
import Data.Text (Text, pack)
import Data.Bifunctor (first)
import Text.Megaparsec (errorBundlePretty, runParserT, pos1)
import Control.Monad.Reader (runReader)
import Interpreter.Syntax.Base
import Interpreter.Parser.Definition
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (partition, foldl', find)
import Interpreter.Span (span)
import Interpreter.Error (ErrorInfo(..), ErrorLevel (PError))
import Interpreter.Stdlib (stdlib)

type Result = (Env Type, [(Name, Expr)], [(Name, Expr)], [Expr])

newtype ParseError = ParseError String

instance ErrorInfo ParseError where
  errorLvl   _ = PError
  errorTitle _ = "Parse error!"

parseSrc :: Text -> Either ParseError Result
parseSrc =
  let s0 = ParseState pos1 Normal
  in first (ParseError . errorBundlePretty) . (`runReader` s0) . runParserT parseSrc' "" . (<> stdlib)

parseSrc' :: Parser Result
parseSrc' = parser >>= either customFailure' (pure . initOpeness) . reduceDefs

isDef :: Def -> Bool
isDef DataDef {} = True
isDef CtorDef {} = True
isDef TypeDef {} = True
isDef _ = False

isFunDef :: Def -> Bool
isFunDef FunDef {} = True
isFunDef _ = False

isConstDef :: Def -> Bool
isConstDef ConstDef {} = True
isConstDef _ = False

isRaw :: Def -> Bool
isRaw Raw {} = True
isRaw _ = False

getDeclOpeness :: DataName -> Env Type -> Openess
getDeclOpeness d@(DataName _ _ o) = maybe o openess . find (sameName d) . Map.keys . dataCtx
  where
    sameName (DataName _ d1 _) (DataName _ d2 _) = d1 == d2
    openess (DataName _ _ o') = o'

initOpeness :: Result -> Result
initOpeness (env, xs, fs, es) = ( replEnv env
                                , fmap replE <$> xs
                                , fmap replE <$> fs
                                , replE <$> es)
  where
  replCInfo :: CtorInfo -> CtorInfo
  replCInfo (CtorInfo lts) = CtorInfo $ (fmap . fmap) replT lts

  replEnv :: Env Type -> Env Type
  replEnv (Env dctx cctx vctx) = Env dctx (replCInfo <$> cctx) (replT <$> vctx)

  replT :: Type -> Type
  replT (TData s d) = TData s $ setOpeness (getDeclOpeness d env) d
  replT (TArr s t1 t2) = TArr s (replT t1) (replT t2)
  replT t = t

  replE :: Expr -> Expr
  replE (Var s n) = Var s n
  replE (Lit s l) = Lit s l
  replE (App s e2 e3 l_e) = App s (replE e2) (replE e3) (replE <$> l_e)
  replE (Lam s xts e) = Lam s (fmap replT <$> xts) (replE e)
  replE (Asc s e t) = Asc s (replE e) (replT t)
  replE (CtorLbl s c args) = CtorLbl s c (map3 replE <$> args)
  replE (CtorPos s c args) = CtorPos s c (replE <$> args)
  replE (Match s e css) = Match s (replE e) (mapCase replE <$> css)
  replE (BinOp s bop e1 e2) = BinOp s bop (replE e1) (replE e2)
  replE (Let s bs e) = Let s (mapBinding replT replE <$> bs) (replE e)
  replE (Access s e l) = Access s (replE e) l
  replE (Ite s e1 e2 e3) = Ite s (replE e1) (replE e2) (replE e3)

map3 :: (a -> b) -> (x,y,a) -> (x,y,b)
map3 f (x,y,a) = (x, y, f a)

reduceDefs :: [Def] -> Either Error Result
reduceDefs ds = do
  let (defs, decls) = partition isDef ds
      env = initEnv defs
  let ks    = mapMaybe (fromConstDef env) decls
      raws  = mapMaybe fromRaw decls
  funs <- mapM (fromFunDef env) $ filter isFunDef decls
  let env' = extendEnv env $ fmap (\x -> TypeDef (fst x) (TUnkn mempty)) (ks <> funs)
  pure (env', funs, ks, reverse raws)

fromRaw :: Def -> Maybe Expr
fromRaw (Raw e) = Just e
fromRaw _ = Nothing

fromFunDef :: Env Type -> Def -> Either Error (Name, Expr)
fromFunDef env (FunDef name xs e) = do
  let t   = lookupVar' name env
      t'  = fromMaybe (TUnkn mempty) t
  (xts, _) <- varsTypes [] xs t'
  let e'  = Lam (span t') xts e
      e'' = Asc (span e) e' t'
  Right (name, e'')
fromFunDef _ _ = Right (Name mempty "", Var mempty (Name mempty ""))

varsTypes :: [(Name, Type)]
  -> [Name]
  -> Type ->
  Either Error ([(Name, Type)], Type)
varsTypes lts [] t = Right (reverse lts, t)
varsTypes lts (l:ls) t@TUnkn{} = varsTypes ((l,t) : lts) ls t
varsTypes lts (l:ls) (TArr _ t1 t2) = varsTypes ((l,t1) : lts) ls t2
varsTypes _ _ t = Left $ EFunType t

fromConstDef :: Env Type -> Def -> Maybe (Name, Expr)
fromConstDef env (ConstDef name e)
  = let t  = lookupVar' name env
        t' = fromMaybe (TUnkn mempty) t
        e' = Asc (span e) e t'
    in Just (name, e')
fromConstDef _ _ = Nothing

findCtorsDef :: Def -> [FoundCtor]
findCtorsDef (FunDef _ _ e) = findCtors e
findCtorsDef (ConstDef _ e) = findCtors e
findCtorsDef (Raw e) = findCtors e
findCtorsDef _ = []

initEnv :: [Def] -> Env Type
initEnv = extendEnv emptyEnv

extendEnv :: Env Type -> [Def] -> Env Type
extendEnv = foldl' populateEnv

populateEnv :: Env Type -> Def -> Env Type
populateEnv (Env dctx cctx vctx) (DataDef d di cs) =
  let dctx' = dctx <> singleton d di
      cctx' = cctx <> fromList cs
   in Env dctx' cctx' vctx
populateEnv (Env dctx cctx vctx) (CtorDef c ci) =
  let cctx' = cctx <> singleton c ci
   in Env dctx cctx' vctx
populateEnv (Env dctx cctx vctx) (TypeDef x t) =
  let vctx' = Map.insertWith (\_ old -> old) x t vctx
   in Env dctx cctx vctx'
populateEnv env _ = env
