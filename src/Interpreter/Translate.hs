module Interpreter.Translate where

import Interpreter.Syntax.Common
import Interpreter.Syntax.Core
import qualified Interpreter.Syntax.EvCore as Ev
import Prelude hiding (span)
import Interpreter.Type
import Interpreter.Span
import Interpreter.Env
import Interpreter.Auxiliary
import Interpreter.Error
import Data.Maybe (fromMaybe)
import Control.Monad.Identity (Identity)

type TransM = EnvM Identity Type () Error

-- * Auxiliary functions

imposibleError :: TransM a
imposibleError = err ImposibleError

-- | Initial evidence
initial :: Type -> Type -> TransM Ev.Evidence
initial t1 t2 = Ev.Evidence <$> cantFail (pure $ meet t1 t2)

-- | Normalize
norm :: Ev.Expr -> Type -> Type -> TransM Ev.Expr
norm e@Ev.Asc{} t1 t2
  | t1 == t2 = pure e
  | otherwise = do
    ev' <- initial t1 t2
    pure $ Ev.Asc ev' e t2
norm e t1 t2 = do
    ev' <- initial t1 t2
    pure $ Ev.Asc ev' e t2

-- | Types to data
todata :: Type -> TransM Type
todata (TUnkn sp) = pure $ TUnknData sp
todata t
  | isData t = pure t
  | otherwise = err $ errConsistency (span t) t (TUnknData $ span t)

-- * Translation function

translate :: Env Type -> Expr -> Either Error Ev.Expr
translate env = fmap fst . fst . evalEnvM env . translateExpr

translateExpr :: Expr -> TransM (Ev.Expr, Type)
translateExpr e | isValue e = do
  (e', t) <- translateValue e
  ev <- initial t t
  pure (Ev.Asc ev e' t, t)
translateExpr (Var sp name) = do
  t <- lookupVar name
  pure (Ev.Var name, fromMaybe (TUnkn sp) t)
translateExpr (App sp e1 e2) = do
  (e1', t1) <- translateExpr e1
  (e2', t2) <- translateExpr e2
  t11 <- dom sp t1
  t12 <- cod sp t1
  e1'' <- norm e1' t1 (TArr (span t1) t11 t12)
  e2'' <- norm e2' t2 t11
  pure (Ev.App e1'' e2'', t12)
translateExpr (Asc _ e t) = do
  (e', t') <- translateExpr e
  ev <- initial t' t
  pure (Ev.Asc ev e' t, t)
translateExpr (Ctor sp c args) = do -- Not value
  t     <- cty sp c
  args' <- mapM (translateCtorArg c) args
  pure (Ev.Ctor c args', t)
translateExpr (BinOp _ bop e1 e2) = do
  (e1', t1') <- translateExpr e1
  (e2', t2') <- translateExpr e2
  let (t1, t2, t) = binOpType bop
  e1''       <- norm e1' t1' t1
  e2''       <- norm e2' t2' t2
  pure (Ev.BinOp bop e1'' e2'', t)
translateExpr (Access sp e l) = do
  (e', t') <- translateExpr e
  t        <- fty sp l t'
  tdata    <- todata t'
  e''      <- norm e' t' tdata
  pure (Ev.Access e'' l t, t)
translateExpr (Match _ e cs) = do
  (e', t) <- translateExpr e
  tdata   <- todata t
  e''     <- norm e' t tdata
  csts    <- mapM translateCase cs
  let (_, ts) = unzip csts
  t'      <- cantFail . pure $ equate ts
  cs''    <- mapM (\(c, t1) -> normCase c t1 t') csts
  pure (Ev.Match e'' cs'', t')
translateExpr _ = imposibleError -- Just to supress the incomplete matches warning.

translateCtorArg :: CtorName -> CtorArg -> TransM Ev.CtorArg
translateCtorArg c (CtorArg sp l e) = do
  (e', t) <- translateExpr e
  t'      <- lty sp l c
  e''     <- norm e' t t'
  pure $ Ev.CtorArg l e''

translateCase :: Case -> TransM (Ev.Case, Type)
translateCase (Case p e) = do
  xts <- parg p
  (e', t) <- withEnv xts $ translateExpr e
  pure (Ev.Case p e', t)

normCase :: Ev.Case -> Type -> Type -> TransM Ev.Case
normCase (Ev.Case p e) t1 t2 = Ev.Case p <$> norm e t1 t2

translateValue :: Expr -> TransM (Ev.Expr, Type)
translateValue (Lit sp l) = pure (Ev.Lit l, litType sp l)
translateValue (Lam sp x tx e) = do
  (e', t) <- withEnv [(x, tx)] $ translateExpr e
  pure (Ev.Lam x tx e', TArr sp tx t)
translateValue (Ctor sp c args) = do
  t     <- cty sp c
  args' <- mapM (translateCtorArg c) args
  pure (Ev.Ctor c args', t)
translateValue _ = imposibleError
