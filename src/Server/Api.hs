{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Server.Api (API, api, SourceCode(..), Response(..)) where

import Data.Aeson
import Servant
import Interpreter.Syntax.EvCore (Expr)
import Data.Text (Text)
import GHC.Generics (Generic)
import Interpreter.Syntax.Common (Valid(..))

type API = "gsd" :> "api" :> "eval" :> ReqBody '[JSON] SourceCode :> Post '[JSON] Response
      :<|> "gsd" :> "api" :> "check" :> ReqBody '[JSON] SourceCode :> Post '[JSON] Response

api :: Proxy API
api = Proxy

data Response
  = Ok    { val   :: Text, typ :: Text, steps :: [Text] }
  | Warn  { title :: Text, msg :: Text, steps :: [Text] }
  | Err   { title :: Text, msg :: Text, steps :: [Text] }
  | PErr  { title :: Text, msg :: Text, steps :: [Text] }
  deriving (Eq, Show, Generic)
instance ToJSON Response

data SourceCode = SourceCode {sourceCode :: Text, valid :: Valid, trace :: Bool}
  deriving (Generic)
instance FromJSON SourceCode
