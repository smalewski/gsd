{-# LANGUAGE DeriveGeneric #-}
module Interpreter.Syntax.Common where

import Interpreter.Span
import Data.Text (Text, unpack)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

-- | Constants
data Lit = LInt Int | LString String
         deriving (Eq, Show)

data Name = Name Span Text
data CtorName = CtorName Span Text
data LabelName = LabelName Span Text

instance Show Name where
  show (Name s x) = unpack x

instance Show CtorName where
  show (CtorName _ x) = unpack x

instance Show LabelName where
  show (LabelName _ x) = unpack x

instance Eq Name where
  (Name _ x) == (Name _ y) = x == y

instance Eq CtorName where
  (CtorName _ x) == (CtorName _ y) = x == y

instance Eq LabelName where
  (LabelName _ x) == (LabelName _ y) = x == y

instance Ord Name where
  compare (Name _ x) (Name _ y) = compare x y

instance Ord CtorName where
  compare (CtorName _ x) (CtorName _ y) = compare x y

instance Ord LabelName where
  compare (LabelName _ x) (LabelName _ y) = compare x y

instance HasSpan Name where
  span (Name s _) = s

instance HasSpan CtorName where
  span (CtorName s _) = s

instance HasSpan LabelName where
  span (LabelName s _) = s

data Pattern
  -- | Constructor patterns
  = CtorP Span CtorName [Name]
  -- | Arity patterns
--  | ArityP Span Name [Name]
  -- | Default pattern
  | DefP Span
  deriving (Eq, Show)

pvar :: Pattern -> [Name]
pvar (CtorP _ _ xs)  = xs
--pvar (ArityP _ _ xs) = xs
pvar (DefP _)        = []

data Openess = Open | Closed
  deriving (Eq, Show)

data BinOp
  = Plus
  | Minus
  | Times
  | Div
  | Equal
  deriving (Eq, Show)

instance HasSpan Pattern where
  span (CtorP sp _ _)  = sp
--  span (ArityP sp _ _) = sp
  span (DefP sp)       = sp

data Valid
  = Sound
  | Exact
  | Complete
  deriving (Eq, Show, Generic)
instance FromJSON Valid
