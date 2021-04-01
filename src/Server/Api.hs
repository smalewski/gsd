{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Server.Api (API, api, SourceCode(..), Response(..), Judgment(..)) where

import Data.Aeson
import Servant
import Interpreter.Syntax.EvCore (Expr)
import Data.Text (Text)
import GHC.Generics (Generic)
import Interpreter.Env (Valid(..))

type API = "eval" :> ReqBody '[JSON] SourceCode :> Post '[JSON] Response
      :<|> "check" :> ReqBody '[JSON] SourceCode :> Post '[JSON] Response

api :: Proxy API
api = Proxy

data Judgment = Judgment { expression :: Text, premises :: [Judgment] }
  deriving (Eq, Show, Generic)
instance ToJSON Judgment

data Response
  = Ok    { val   :: Judgment, steps :: [Judgment] }
  | Warn  { title :: Text, msg   :: Text   }
  | Err   { title :: Text, msg   :: Text   }
  | PErr  { title :: Text, msg   :: Text   }
  deriving (Eq, Show, Generic)
instance ToJSON Response

data SourceCode = SourceCode {sourceCode :: Text, valid :: Valid}
  deriving (Generic)
instance FromJSON SourceCode
