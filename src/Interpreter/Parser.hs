{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TupleSections #-}
module Interpreter.Parser where

import Prelude hiding (span)
import Data.Map.Strict (Map, singleton, fromList)
import qualified Data.Map.Strict as Map
import Interpreter.Env
import Interpreter.Parser.Helpers hiding (ParseError)
import Interpreter.Syntax.Common
import Interpreter.Type
import qualified Interpreter.Parser.Definition.Data as DefData
import Data.Text (Text, pack)
import Data.Bifunctor (first)
import Text.Megaparsec (errorBundlePretty, parse, runParserT, pos1, customFailure)
import Control.Monad.Reader (runReader)
import Interpreter.Syntax.Base
import Interpreter.Parser.Definition
import Control.Monad (join, void)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (sortOn, partition, sort, foldl')
import Interpreter.Span (span)
import Interpreter.Error (ErrorTxt(errorTxt))
import Interpreter.Printer (ppr)
import Data.Foldable (foldlM)
import Interpreter.Stdlib (stdlib)

type Result = (Env Type, [(Name, Expr)], [(Name, Expr)], [Expr])

newtype ParseError = ParseError String

instance ErrorTxt ParseError where
  errorTxt (ParseError s) = (Nothing, pErr, pack s)

pErr :: Text
pErr = "Parsing Error"

parseSrc :: Text -> Either ParseError Result
parseSrc =
  let s0 = ParseState pos1 Normal
  in first (ParseError . errorBundlePretty) . (`runReader` s0) . runParserT parseSrc' "" . (<> stdlib)

parseSrc' :: Parser Result
parseSrc' = parser >>= either customFailure' pure . reduceDefs

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

reduceDefs :: [Def] -> Either Error Result
reduceDefs ds = do
  let (defs, decls) = partition isDef ds
      env = initEnv defs
      cs  = concatMap findCtorsDef decls
  env' <- pure env --extendEnvWithUnclass env cs
  let ks    = mapMaybe (fromConstDef env') decls
      raws  = mapMaybe fromRaw decls
  funs <- mapM (fromFunDef env') $ filter isFunDef decls
  let env'' = extendEnv env' $ fmap (\x -> TypeDef (fst x) (TUnkn mempty)) (ks <> funs)
  pure (env'', funs, ks, reverse raws)

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
fromFunDef _ _ = Right (Name mempty "LOL", Var mempty (Name mempty "LOL"))

varsTypes :: [(Name, Type)]
  -> [Name]
  -> Type ->
  Either Error ([(Name, Type)], Type)
varsTypes lts [] t = Right (reverse lts, t)
varsTypes lts (l:ls) t@(TUnkn s) = varsTypes ((l,t) : lts) ls t
varsTypes lts (l:ls) (TArr s t1 t2) = varsTypes ((l,t1) : lts) ls t2
varsTypes _ _ t = Left $ EFunType t

fromConstDef :: Env Type -> Def -> Maybe (Name, Expr)
fromConstDef env (ConstDef name e)
  = let t  = lookupVar' name env
        t' = fromMaybe (TUnkn mempty) t
        e' = Asc (span e) e t'
    in Just (name, e')
fromConstDef _ _ = Nothing

extendEnvWithUnclass :: Env Type
  -> [FoundCtor]
  -> Either Error (Env Type)
extendEnvWithUnclass env fcs = foldlM matchArity env (sort fcs)
  where
    insertUnclass :: Env a -> CtorName -> [LabelName] -> Env a
    insertUnclass env c ls = let cinfo = CtorInfo $ zip ls (repeat $ TUnkn mempty)
                                 env'  = insertCtor env c cinfo
                             in extendData env' Nothing c

    matchArity :: Env Type -> FoundCtor -> Either Error (Env Type)
    matchArity env (FCtor c norls) =
      case (lookupCtor' c env, norls) of
        (Nothing, Left n)   -> Left  $ EPosUnclass c
        (Nothing, Right ls) -> Right $ insertUnclass env c ls
        (Just ci, Left n)
          | (length . argTypes) ci == n -> Right env
          | otherwise                   -> Left $ EArity c
        (Just ci, Right ls)
          | sort ls == sort (fmap fst . argTypes $ ci) -> Right env
          | otherwise                                  -> Left $ ELabels c

    matchArity env (FPat c n) =
      case lookupCtor' c env of
        Nothing -> let CtorName sp txt = c
                       lns             = [ txt <> pack (show i) | i <- [1..n] ]
                       ls              = LabelName sp <$> lns
                   in Right $ insertUnclass env c ls
        Just ci
          | (length . argTypes) ci == n -> Right env
          | otherwise                   -> Left $ EArity c

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
