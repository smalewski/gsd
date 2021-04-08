{-# LANGUAGE OverloadedStrings #-}

module Server where

import Data.Data
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Servant.Options
import Servant
import Servant.Server
import Server.Api
import Interpreter
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, unpack)
import Data.Functor (($>))

app :: Application
app =
  logStdoutDev $ cors (const $ Just policy) $ provideOptions api $ serve api server
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = ["content-type"] }

server :: Server API
server = evalH :<|> checkH
  where
    evalH :: SourceCode -> Handler Response
    evalH (SourceCode src valid trace) = liftIO (run valid trace src >>= buildResponse)

    checkH :: SourceCode -> Handler Response
    checkH (SourceCode src valid _) = liftIO (check valid src >>= buildResponse)

buildResponse :: Either ErrorText ResultText -> IO Response
buildResponse (Right (ResultText (value, stack))) = pure $ Ok (dummyJudgment value) (dummyJudgment <$> stack)
buildResponse (Left (ErrorText (Warning, title, msg))) = pure $ Warn title msg
buildResponse (Left (ErrorText (Error, title, msg))) = pure $ Err title msg
buildResponse (Left (ErrorText (PError, title, msg))) = putStrLn (unpack msg) $> PErr title msg

dummyJudgment :: Text -> Judgment
dummyJudgment txt = Judgment txt []
