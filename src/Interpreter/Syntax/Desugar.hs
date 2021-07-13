{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Syntax.Desugar (desugar) where

import Prelude hiding (span)

import Interpreter.Type
import Interpreter.Syntax.Base
import qualified Interpreter.Syntax.Core as C
import Interpreter.Syntax.Common

import Interpreter.Error
import Interpreter.Env

import Interpreter.Span
import Data.Map.Strict (assocs)
import Control.Monad.Except (runExceptT)
import Control.Monad.Writer (runWriterT)
import Control.Monad.Reader (runReaderT)
import Data.Bifunctor (second)
import Control.Monad.Identity (Identity)

type DesugarM = EnvM Identity Type () Error

desugar :: Env Type -> Expr -> Either Error C.Expr
desugar env = fst . evalEnvM env . desugarExpr

desugarExpr :: Expr -> DesugarM C.Expr
desugarExpr (Var sp name) = pure $ C.Var sp name
desugarExpr (Lit sp lit) = pure $ C.Lit sp lit
desugarExpr (App sp e1 e2 es) = do
  e'  <- desugarExpr e1
  es' <- mapM desugarExpr (e2 : es)
  pure $ foldl (C.App sp) e' es'
desugarExpr (Lam sp args e) = do
  e' <- desugarExpr e
  pure $ foldr (uncurry $ C.Lam sp) e' args
desugarExpr (CtorLbl sp cname args) = do
  args' <- mapM desugarArg args
  pure $ C.Ctor sp cname args'
desugarExpr (CtorPos sp cname args) = do
  ls <- ctorLabels cname >>= maybe (err $ CtorNotFoundError sp cname) pure
  args' <- mapM desugarExpr args
  let args'' = zipWith (\l e -> C.CtorArg (span e) l e) ls args'
  pure $ C.Ctor sp cname args''
desugarExpr (Match sp e cases) = do
  e' <- desugarExpr e
  cases' <- mapM desugarCase cases
  pure $ C.Match sp e' cases'
desugarExpr (BinOp sp bop e1 e2) = do
  e1' <- desugarExpr e1
  e2' <- desugarExpr e2
  pure $ C.BinOp sp bop e1' e2'
desugarExpr (Ite sp cond ifTrue ifFalse) = do
  ifTrue'  <- desugarExpr ifTrue
  ifFalse' <- desugarExpr ifFalse
  cond'    <- desugarExpr cond
  let caseFalse = C.Case (CtorP (span ifFalse) (CtorName (span ifFalse) "False") []) ifFalse'
      caseTrue = C.Case (CtorP (span ifTrue) (CtorName (span ifTrue) "True") []) ifTrue'
  pure $ C.Match sp cond' [caseTrue, caseFalse]
desugarExpr (Access sp e l) = do
  e' <- desugarExpr e
  pure $ C.Access sp e' l
desugarExpr (Asc sp e ty) = do
  e' <- desugarExpr e
  pure $ C.Asc sp e' ty
desugarExpr (Let sp bs e) = do
  e'  <- desugarExpr e
  bs' <- (mapM . mapM) desugarExpr bs
  pure $ foldr (\(LetBinding sp x t e) acc -> C.App sp (C.Lam sp x t acc) e) e' bs'


desugarArg :: (Span, LabelName, Expr) -> DesugarM C.CtorArg
desugarArg (sp, l, e) = C.CtorArg sp l <$> desugarExpr e

desugarCase :: Case -> DesugarM C.Case
desugarCase (Case pat e) = C.Case pat <$> desugarExpr e
