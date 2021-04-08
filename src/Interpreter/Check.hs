{-# LANGUAGE OverloadedStrings#-}

module Interpreter.Check where

import           Prelude hiding (span)

import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Function (on)
import           Data.List (sort, unzip4)

import           Interpreter.Type
import           Interpreter.Span
import           Interpreter.Env
import           Interpreter.Error
import           Interpreter.Syntax.Common
import           Interpreter.Syntax.Core
import qualified Interpreter.Syntax.Core as Ev
import Interpreter.Auxiliary (cod, dom, cty, lty, satisfyLabels, valid, equate, fty, parg, meet, precise)
import Data.Maybe (isJust)
import Control.Monad.Identity (Identity)

type CheckM = EnvM Identity Type () Error

typecheck :: Valid -> Env Type -> Expr -> Either Error Type
typecheck valid env = fmap fst . evalEnvM env . typecheckExpr valid

wfEnv :: Env Type -> Either Error ()
wfEnv env = void $ evalEnvM env (wfDataCtx *> wfCtorCtx *> wfTypeCtx)

typecheckExpr :: Valid -> Expr -> CheckM Type
typecheckExpr _ (Var sp x) =
  lookupVar x >>= maybe (err $ VarNotFoundError sp x) pure
typecheckExpr _ (Lit sp l) = pure $ litType sp l
typecheckExpr v (App sp e1 e2) = do
  t1  <- typecheckExpr v e1
  t2  <- typecheckExpr v e2
  t11 <- dom (span e1) t1
  t12 <- cod (span e1) t1
  t2 ~~ t11
  pure t12
typecheckExpr v (Lam sp x tx e) = do
  t <- withEnv [(x, tx)] $ typecheckExpr v e
  pure $ TArr (span tx) tx t
typecheckExpr v (Asc sp e t') = do
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
  equate ts >>= maybe (err $ MatchTypesError sp) pure
typecheckExpr v (BinOp sp bop e1 e2) = do
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
bopType Equal _ _ = pure $ TData mempty (DataName mempty "Bool")
bopType _ t1 t2 = do
  t1 ~~ intT
  t2 ~~ intT
  pure intT
  where
    intT = TBase mempty TInt

consistency :: Type -> Type -> CheckM ()
consistency (TBase _ b1) (TBase _ b2) | b1 == b2 = pass
consistency t@(TData _ d1) (TData _ d2) | d1 == d2 = wfT t
consistency (TArr _ t1 t2) (TArr _ t3 t4)
  = consistency t1 t3 >> consistency t2 t4
consistency t@TUnclass{} TUnclass{} = wfT t
consistency t1@(TData _ d) t2@TUnclass{} = do
  wfT t1
  oss <- openess d
  case oss of
    Nothing     -> err $ errDataNotFound (span t1) d
    Just Closed -> err $ errConsistency (span t1) t1 t2
    Just Open   -> pass
consistency t1@TUnclass{} t2@TData{} = consistency t2 t1
consistency TUnknData{} TUnknData{}  = pass
consistency TUnknData{} t@TUnclass{} = wfT t
consistency t@TUnclass{} TUnknData{} = wfT t
consistency t@TData{} TUnknData{}    = wfT t
consistency TUnknData{} t@TData{}    = wfT t
consistency TUnkn{} t                = wfT t
consistency t TUnkn{}                = wfT t
consistency t1 t2 = err $ errConsistency (span t1 <> span t2) t1 t2

(~~) :: Type -> Type -> CheckM ()
(~~) = consistency

wfDataCtx :: CheckM ()
wfDataCtx = pass

wfCtorCtx :: CheckM ()
wfCtorCtx = pass

wfTypeCtx :: CheckM ()
wfTypeCtx = pass

wfT :: Type -> CheckM ()
wfT (TBase _ _) = pass
wfT (TData sp d) = wfDataCtx *> lookupData (Just d) *> pass
wfT (TArr sp t1 t2) = wfT t1 *> wfT t2
wfT (TUnkn sp) = wfDataCtx
wfT (TUnknData sp) = do
  wfDataCtx
  datas <- dumpDataCtx
  if null datas then err $ NoDataError sp else pass
wfT (TUnclass sp) = do
  wfDataCtx
  let onlyOpen (d, di) = isJust d && Open == di_openess di
  openDatas <- filter onlyOpen <$> dumpDataCtx
  if null openDatas then err $ NoOpenDataError sp else pass

