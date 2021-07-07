{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpreter.Syntax.EvCore where

import Prelude hiding (span)
import Interpreter.Type
import Interpreter.Syntax.Common
import Data.IORef (IORef)
import Interpreter.Env (Env)
import Data.Aeson (Result, FromJSON (parseJSON), Object, ToJSON (toJSON))
import qualified Data.Aeson as A
import Data.HashMap.Strict (toList, size)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Scientific (scientific, coefficient)

-- | Expressions
data Expr
  -- | Variables
  = Var Name
  -- | Constants
  | Lit Lit
  -- | Funtion application
  | App Expr Expr
  -- | Lambdas
  | Lam Name Type Expr
  -- | Ascriptions
  | Asc Evidence Expr Type
  -- | Constructor
  | Ctor CtorName [CtorArg]
  -- | Pattern matching
  | Match Expr [Case]
  -- | Binary operator
  | BinOp BinOp Expr Expr
  -- | Field access
  | Access Expr LabelName Type
  -- | Value
  | Value Evidence Expr Type
  -- | Value
  | FromJson
  -- | Value
  | ToJson
  -- | Value
  | Clos Name Type Expr (Env Value)
  -- | Highlight
  | Highlight Expr
  deriving (Eq, Show)

data Value
  = Boxed (IORef Expr)
  | Unboxed Expr

instance Eq Value where
  _ == _ = False

instance Show Value where
  show (Boxed _) = "Boxed"
  show (Unboxed e) = "U " <> show e


-- | Cases of a match expression
data Case = Case { casePattern :: Pattern, caseExpr :: Expr }
  deriving (Eq, Show)

newtype Evidence = Evidence Type
  deriving (Eq, Show)

data CtorArg = CtorArg { ctorArgLabel :: LabelName
                       , ctorArgExpr :: Expr
                       } deriving (Eq, Show)

unfoldArgs :: [CtorArg] -> ([LabelName], [Expr])
unfoldArgs [] = ([], [])
unfoldArgs (CtorArg l e : cargs) =
  let (ls, es) = unfoldArgs cargs
  in  (l:ls, e:es)

isValue :: Expr -> Bool
isValue Value {} = True
isValue _        = False

isRawValue :: Expr -> Bool
isRawValue Lit{}       = True
isRawValue Clos{}      = True
isRawValue (Ctor _ xs) = all isValue . fmap ctorArgExpr $ xs
isRawValue ToJson      = True
isRawValue FromJson    = True
isRawValue _           = False

newtype CtorArgs = CtorArgs { fromCtorArgs :: [CtorArg]}

instance FromJSON CtorArgs where
  parseJSON (A.Object v) = do
    let f (x,o) = CtorArg (LabelName mempty x) <$> parseJSON o
    CtorArgs <$> mapM f (toList v)
  parseJSON _ = fail "parsing ctorargs failed"

instance FromJSON Expr where
  parseJSON (A.Object v)
    | size v == 1 = do
        let [(c, o)] = toList v
            cname = CtorName mempty c
        args <- fromCtorArgs <$> parseJSON o
        pure $ Ctor cname args

    | otherwise = fail "parsing ctor failed"
  parseJSON (A.String txt) = pure $ strValue txt
  parseJSON (A.Number n) = pure . numValue . fromEnum $ coefficient n
  parseJSON (A.Bool b) = pure $ boolValue b
  parseJSON _ = fail "can't parse json"

strValue :: Text -> Expr
strValue txt = Value ev (Lit $ LString txt) t
  where
    t  = TBase mempty TString
    ev = Evidence t

numValue :: Int -> Expr
numValue n = Value ev (Lit $ LInt n) t
  where
    t  = TBase mempty TInt
    ev = Evidence t

ctorTrue :: Expr
ctorTrue = Ctor (CtorName mempty "True") []

ctorFalse :: Expr
ctorFalse = Ctor (CtorName mempty "False") []

boolValue :: Bool -> Expr
boolValue b = let c = if b then "True" else "False"
                  cname = CtorName mempty c
                  ctor = Ctor cname []
                  t = TData mempty (DataName mempty "Bool" Closed)
                  ev = Evidence t
              in Value ev ctor t

pattern ETrue <- Ctor (CtorName _ "True") []
pattern EFalse <- Ctor (CtorName _ "False") []

instance ToJSON Expr where
  toJSON (Value _ e _) = toJSON e
  toJSON (Lit (LInt n)) = A.Number (scientific (toInteger n) 0)
  toJSON (Lit (LString txt)) = A.String txt
  toJSON ETrue = A.Bool True
  toJSON EFalse = A.Bool False
  toJSON (Ctor (CtorName _ c) args) = A.object [(c, toJSON $ CtorArgs args)]
  toJSON _ = A.Null

instance ToJSON CtorArgs where
  toJSON = A.object . fmap (\(CtorArg (LabelName _ l) v) -> (l, toJSON v)) . fromCtorArgs

