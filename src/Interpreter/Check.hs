{-# LANGUAGE OverloadedStrings#-}

module Interpreter.Check where

import           Prelude hiding (span)

import           Control.Monad
import           Interpreter.Type
import           Interpreter.Span
import           Interpreter.Env
import           Interpreter.Error
import           Interpreter.Syntax.Common
import           Interpreter.Syntax.Core
import Interpreter.Auxiliary (cod, dom, cty, lty, satisfyLabels, valid, equate, fty, parg)
import Control.Monad.Identity (Identity)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowM)

type CheckM = EnvM Identity Type () Error

typecheck :: Valid -> Env Type -> Expr -> Either Error Type
typecheck valid' env = fmap fst . evalEnvM env . typecheckExpr valid'

wfEnv :: Env Type -> Either Error ()
wfEnv env = void $ evalEnvM env wfTypeCtx

typecheckExpr :: Valid -> Expr -> CheckM Type
typecheckExpr _ (Var sp x) =
  lookupVar x >>= maybe (err $ VarNotFoundError sp x) pure
typecheckExpr _ (Lit sp l) = pure $ litType sp l
typecheckExpr v (App _ e1 e2) = do
  t1  <- typecheckExpr v e1
  t2  <- typecheckExpr v e2
  t11 <- dom (span e1) t1
  t12 <- cod (span e1) t1
  t2 ~~ t11
  pure t12
typecheckExpr v (Lam _ x tx e) = do
  t <- withEnv [(x, tx)] $ typecheckExpr v e
  pure $ TArr (span tx) tx t
typecheckExpr v (Asc _ e t') = do
  t <- typecheckExpr v e
  t ~~ t'
  pure t'
typecheckExpr v (Ctor sp c args) = do
  t <- cty sp c
  t ~~ TUnknData (span t)
  let (sps, ls, exprs) = unfoldArgs args
  ts <- mapM (typecheckExpr v) exprs
  ts' <- zipWithM (\s l-> lty s l c) sps ls
  zipWithM_ (~~) ts ts'
  satisfyLabels sp c ls
  pure t
typecheckExpr v (Match sp e cs) = do
  t <- typecheckExpr v e
  t ~~ TUnknData (span t)
  let pats = casePattern <$> cs
  valid v pats t
  ts <- mapM (typecheckCase v) cs
  maybe (err $ MatchTypesError sp) pure (equate ts)
typecheckExpr v (BinOp _ bop e1 e2) = do
  t1 <- typecheckExpr v e1
  t2 <- typecheckExpr v e2
  bopType bop t1 t2
typecheckExpr v (Access sp e l) = do
  t <- typecheckExpr v e
  t ~~ TUnknData (span t)
  fty sp l t

typecheckCase :: Valid -> Case -> CheckM Type
typecheckCase v (Case pat e) = do
  xts <- parg pat
  withEnv xts $ typecheckExpr v e

bopType :: BinOp -> Type -> Type -> CheckM Type
bopType Equal _ _ = pure $ TData mempty (DataName mempty "Bool" Closed)
bopType _ t1 t2 = do
  t1 ~~ intT
  t2 ~~ intT
  pure intT
  where
    intT = TBase mempty TInt

consistency :: Type -> Type -> CheckM ()
consistency (TBase _ b1)   (TBase _ b2) | b1 == b2 = pass
consistency (TData _ d1)   (TData _ d2) | d1 == d2 = pass
consistency (TData _ d)    TUnclass{}  | isOpen d = pass
consistency TUnclass{}     (TData _ d) | isOpen d = pass
consistency (TArr _ t1 t2) (TArr _ t3 t4) = consistency t1 t3 >> consistency t2 t4
consistency TUnclass{}     TUnclass{}  = pass
consistency TUnknData{}    TUnknData{} = pass
consistency TUnknData{}    TUnclass{}  = pass
consistency TUnclass{}     TUnknData{} = pass
consistency TData{}        TUnknData{} = pass
consistency TUnknData{}    TData{}     = pass
consistency TUnkn{}        _           = pass
consistency _              TUnkn{}     = pass
consistency t1 t2 = err $ errConsistency (span t1 <> span t2) t1 t2

(~~) :: Type -> Type -> CheckM ()
(~~) = consistency

wfDataCtx :: CheckM ()
wfDataCtx = do
  dns <- fmap (S.fromList . di_ctors <$>) <$> dumpDataCtx
  let mDup = dupData $ fst <$> dns
  maybe pass (err . errDuplicatedData) mDup
  let ctorSets = snd <$> dns
      allCtors = S.unions ctorSets
  when (sum (S.size <$> ctorSets) /= S.size allCtors) (err errDuplicatedCtor)

dupData :: [DataName] -> Maybe DataName
dupData []  = Nothing
dupData [_] = Nothing
dupData ((DataName _ d1 _) : x@(DataName _ d2 _) : xs)
  | d1 == d2  = Just x
  | otherwise = dupData (x:xs)

wfCtorCtx :: CheckM ()
wfCtorCtx = do
  wfDataCtx
  ltss <- fmap (argTypes <$>) <$> dumpCtorCtx
  mapM_ (uncurry wfCtor) ltss

wfCtor :: CtorName -> [(LabelName, Type)] -> CheckM ()
wfCtor c lts = do
  let ls = fst <$> lts
      ts = snd <$> lts
      labelSet = S.fromList ls
      sp = span c
  when (S.size labelSet /= length lts) (err $ errDuplicatedLabels c)
  d   <- cty (span c) c
  ts' <- mapM (\l -> errorMaybeH errHandler $ fty sp l d) ls
  let ts'' = fromMaybe (TUnkn mempty) <$> ts'
  zipWithM_ (~~) ts ts''

errHandler :: Error -> CheckM (Maybe a)
errHandler e@LabelNotConsistentError{} = err e
errHandler _ = pure Nothing

wfTypeCtx :: CheckM ()
wfTypeCtx = do
  wfCtorCtx
  ts <- fmap snd <$> dumpVarCtx
  mapM_ wfT ts

-- wfDataCtx was already checked, no need to recheck it
wfT :: Type -> CheckM ()
wfT (TBase _ _) = pass
wfT (TData _ d) = existsData d
wfT (TArr _ t1 t2) = wfT t1 *> wfT t2
wfT (TUnkn _) = pass
wfT (TUnknData _) = pass
wfT (TUnclass _) = pass

existsData :: DataName -> CheckM ()
existsData d = do
  env <- dumpDataCtx
  if any ((== d) . fst) env
    then pass
    else err $ errDataNotFound (span d) d
