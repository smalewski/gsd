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
import Control.Monad.Except (ExceptT, runExceptT, MonadIO (liftIO), withExceptT, mapExceptT, join)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Interpreter.Syntax.Common (Valid)
import qualified Interpreter.Eval as E
import qualified Interpreter.Error as T
import Data.Bifunctor (second, first)

data Res a = Res { expr :: a, typ :: Type, trace :: [a] }
           | Err { err :: OutError, trace :: [a] }

check :: Valid -> Text -> IO (Res B.Expr)
check valid src = fmap makeRes . runExceptT $ do
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

  pure (Right e, t, [])

run :: Valid -> Bool -> Text -> IO (Res Ev.Expr)
run valid trace src = fmap makeRes . runExceptT $ do
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
  (rT, stack) <- liftIO $ first withErr <$> eval trace valEnv e'
  let r = errPriority $ runExceptT rT

  pure (r, t, stack)

errPriority :: Either e (Either e a) -> Either e a
errPriority (Left e) = Left e
errPriority (Right (Left e)) = Left e
errPriority (Right (Right a)) = Right a

makeRes :: Either OutError (Either OutError a, Type, [a]) -> Res a
makeRes (Left e)          = Err e []
makeRes (Right (r, t, s)) =
  case r of
    Left e  -> Err e s
    Right v -> Res v t s

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

--  (Either Error Expr, [Expr]) -> (ExceptT ErrRes m Expr, [Expr])

note :: a -> Maybe b -> Either a b
note x Nothing = Left x
note _ (Just x) = Right x
