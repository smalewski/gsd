{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import Data.Text (Text)
import Interpreter.Parser (parseSrc, ParseError)
import Interpreter.Syntax.Desugar (desugar)
import Interpreter.Error (ErrorTxt (errorTxt), Error)
import Interpreter.Printer (ppr)
import Interpreter.Check (wfEnv, typecheck)
import Data.Maybe (listToMaybe)
import Interpreter.Translate (translate)
import Interpreter.Eval (initValEnv, eval)
import qualified Interpreter.Eval as Eval
import Debug.Trace (trace, traceM)
import Control.Monad.Except (ExceptT, runExceptT, MonadIO (liftIO), withExceptT)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Interpreter.Syntax.Common (Valid)

data ErrorLevel = PError | Error | Warning

newtype ErrorText = ErrorText (ErrorLevel, Text, Text)

newtype ResultText = ResultText (Text, [Text])

check :: Valid -> Text -> IO (Either ErrorText ResultText)
check valid src = runExceptT $ do
  traceM "Preparser"
  -- Parse
  (env, fs, ks, es) <- withErr $ parseSrc src
  traceM "Postparser"

  -- Desugar
  es' <- withErr $ mapM (desugar env) es
  fs' <- withErr $ (mapM . mapM) (desugar env) fs
  ks' <- withErr $ (mapM . mapM) (desugar env) ks
  traceM "Postdesugar"

  -- Typecheck
  withErr $ wfEnv env
  traceM "WF"
  ts <- withErr $ mapM (typecheck valid env) es'
  traceM "PostTypecheck Expr"
  withErr $ (mapM_ . mapM_) (typecheck valid env) fs'
  traceM "PostTypecheck Funs"
  withErr $ (mapM_ . mapM_) (typecheck valid env) ks'
  traceM "PostTypecheck Ks"

  -- Select the last raw expression
  (e, t) <- ExceptT . pure . note noRawWarning . listToMaybe $ zip es' ts

  traceM "Preprint"
  -- To text
  let eTxt = ppr e <> "~:~" <> ppr t
  traceM "Postprint"

  pure $ ResultText (trace (show eTxt) eTxt, [])

run :: Valid -> Bool -> Text -> IO (Either ErrorText ResultText)
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
  (e, t) <- ExceptT . pure . note noRawWarning . listToMaybe $ zip es' ts

  -- Translate
  e'   <- withErr $ translate env e
  fs'' <- withErr $ (mapM . mapM) (translate env) fs'
  ks'' <- withErr $ (mapM . mapM) (translate env) ks'

  -- Evaluate
  valEnv     <- withErr' $ initValEnv trace env fs'' ks''
  (v, stack) <- withErrIO $ eval trace valEnv e'

  -- To text
  let vTxt       = ppr v <> "~:~" <> ppr t
      stackTxt   = ppr <$> stack

  pure $ ResultText (vTxt, stackTxt)

noRawWarning :: ErrorText
noRawWarning = ErrorText (Warning, "No expression to evaluate", "\\text{The program typechecked correctly, but there is no expression to evaluate.}")

withErr :: (MonadIO m, ErrorWithLevel a) => Either a b -> ExceptT ErrorText m b
withErr = withExceptT errorWithLevel . ExceptT . pure

withErrIO :: (MonadIO m, ErrorWithLevel a) => IO (Either a b) -> ExceptT ErrorText m b
withErrIO = withExceptT errorWithLevel . ExceptT . liftIO

withErr' :: (MonadIO m, ErrorWithLevel a) => ExceptT a m b -> ExceptT ErrorText m b
withErr' = withExceptT errorWithLevel

note :: a -> Maybe b -> Either a b
note x Nothing = Left x
note _ (Just x) = Right x

class ErrorWithLevel a where
  errorWithLevel :: a -> ErrorText

instance ErrorWithLevel Error where
  errorWithLevel = errorWithLevel'

instance ErrorWithLevel ParseError where
  errorWithLevel x = let (_, title, body) = errorTxt x
                     in ErrorText (PError, title, body)

instance ErrorWithLevel Eval.Error where
  errorWithLevel = errorWithLevel'

errorWithLevel' :: ErrorTxt a => a -> ErrorText
errorWithLevel' x = let (_, title, body) = errorTxt x
                    in ErrorText (Error, title, "\\text{" <> body <> "}")

instance ErrorWithLevel ErrorText where
  errorWithLevel = id
