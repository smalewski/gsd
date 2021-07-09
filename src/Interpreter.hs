{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import Data.Text (Text)
import Interpreter.Parser
import Interpreter.Syntax.Desugar (desugar)
import Interpreter.Error (ErrorInfo(..), ErrorLevel(Warning))
import Interpreter.Check (wfEnv, typecheck)
import Data.Maybe (listToMaybe)
import Interpreter.Translate (translate)
import Interpreter.Eval (initValEnv, eval)
import qualified Interpreter.Eval as Eval
import Interpreter.Type (Type)
import qualified Interpreter.Syntax.EvCore as Ev
import qualified Interpreter.Syntax.Core as B
import Control.Monad.Except (ExceptT, runExceptT, MonadIO (liftIO), withExceptT, mapExceptT)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Interpreter.Syntax.Common (Valid)
import qualified Interpreter.Eval as E
import qualified Interpreter.Error as T
import Data.Bifunctor (second, first)

data Res a = Res { expr :: a, typ :: Type, trace :: [a] }
  deriving (Show)

check :: Valid -> Text -> IO (Either OutError (Res B.Expr))
check valid src = runExceptT $ do
  -- Parse
  (env, fs, ks, es) <- withErr $ parseSrc src

  -- Desugar
  es' <- withErr $ mapM (desugar env) es
  fs' <- withErr $ (mapM . mapM) (desugar env) fs
  ks' <- withErr $ (mapM . mapM) (desugar env) ks

  -- Typecheck
  withErr $ wfEnv env
  ts <- withErr $ mapM (typecheck valid env) es'
  withErr $ (mapM_ . mapM_) (typecheck valid env) fs'
  withErr $ (mapM_ . mapM_) (typecheck valid env) ks'

  -- Select the last raw expression
  (e, t) <- withErr . note NoExprWarning . listToMaybe $ zip es' ts

  pure $ Res e t []

run :: Valid -> Bool -> Text -> IO (Either OutError (Res Ev.Expr))
run valid trace src = runExceptT $ do
  -- Parse
  (env, fs, ks, es) <- withErr $ parseSrc src

  -- Desugar
  es' <- withErr $ mapM (desugar env) es
  fs' <- withErr $ (mapM . mapM) (desugar env) fs
  ks' <- withErr $ (mapM . mapM) (desugar env) ks

  -- Typecheck
  withErr $ wfEnv env
  ts <- withErr $ mapM (typecheck valid env) es'
  withErr $ (mapM_ . mapM_) (typecheck valid env) fs'
  withErr $ (mapM_ . mapM_) (typecheck valid env) ks'

  -- Select the last raw expression
  (e, t) <- withErr . note NoExprWarning . listToMaybe $ zip es' ts

  -- Translate
  e'   <- withErr $ translate env e
  fs'' <- withErr $ (mapM . mapM) (translate env) fs'
  ks'' <- withErr $ (mapM . mapM) (translate env) ks'

  -- Evaluate
  valEnv     <- withExceptT toOutError $ initValEnv trace env fs'' ks''
  (v, stack) <- withErrM $ eval trace valEnv e'

  pure $ Res v t stack


data Error = NoExprWarning

instance ErrorInfo Error where
  errorLvl NoExprWarning = Warning
  errorTitle NoExprWarning = "No expression to evaluate."

data OutError
  = IE Error
  | EE E.Error
  | PE ParseError
  | TE T.Error

instance ErrorInfo OutError where
  errorLvl (IE e) = errorLvl e
  errorLvl (EE e) = errorLvl e
  errorLvl (PE e) = errorLvl e
  errorLvl (TE e) = errorLvl e

  errorTitle (IE e) = errorTitle e
  errorTitle (EE e) = errorTitle e
  errorTitle (PE e) = errorTitle e
  errorTitle (TE e) = errorTitle e

class IsError a where
  toOutError :: a -> OutError

instance IsError Error where
  toOutError = IE

instance IsError E.Error where
  toOutError = EE

instance IsError ParseError where
  toOutError = PE

instance IsError T.Error where
  toOutError = TE

withErr :: (Monad m, IsError e) => Either e a -> ExceptT OutError m a
withErr = withErrM . pure

withErrM :: (Monad m, IsError e) => m (Either e a) -> ExceptT OutError m a
withErrM = withExceptT toOutError . ExceptT

note :: a -> Maybe b -> Either a b
note x Nothing = Left x
note _ (Just x) = Right x
