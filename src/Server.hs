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
import Interpreter.Error (ErrorLevel(..), ErrorInfo(..))
import Interpreter.Printer.Latex (PrintableLatex(ppr))
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
    evalH (SourceCode src valid trace) = liftIO (buildResponse <$> run valid trace src)

    checkH :: SourceCode -> Handler Response
    checkH (SourceCode src valid _) = liftIO (buildResponse <$> check valid src)

buildResponse :: PrintableLatex a => Either OutError (Res a) -> Response
buildResponse (Right (Res e t st)) = Ok (ppr e) (ppr t) (ppr <$> st)
buildResponse (Left e) =
  let title = errorTitle e
      msg = ppr e
      ctor = case errorLvl e of
              PError  -> PErr
              Error   -> Err
              Warning -> Warn
  in ctor title msg
