{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpreter.Error where

import Prelude hiding (span)
import Interpreter.Span
import Interpreter.Syntax.Common
import Interpreter.Type
import Data.Text (Text)

class IsError a where
  errCtorNotFound :: Span -> CtorName -> a
  errDataNotFound :: Span -> DataName -> a
  errConsistency :: Span -> Type -> Type -> a
  errLabelNotConsistent :: Span -> LabelName ->  Type -> a
  errNoLabel :: Span -> LabelName ->  Type -> a
  errNoCtorLabel :: Span -> LabelName ->  CtorName -> a
  errInvalidLabels :: Span -> CtorName -> a
  errInvalidMatch :: Span -> Valid -> Type -> a
  errDuplicatedData :: DataName -> a
  errDuplicatedCtor :: a
  errDuplicatedLabels :: CtorName -> a
  errImposible :: a

-- | Type checking errors
data Error
  -- | Type error
  = TypeError Span
  -- | Unbound variable errors
  | VarNotFoundError Span Name
  -- | Constructor not found error
  | CtorNotFoundError Span CtorName
  -- | Datatype not found error
  | DataNotFoundError Span DataName
  -- | Datatype is closed error
  | DataClosedError Span DataName
  -- | Partial function not defined error
  | PartialFunctionError Span String Type
  -- | Predicate doesn't hold
  | PredicateError Span String
  -- | Consistency Error
  | ConsistencyError Span Type Type
  -- | Label has not a consistent type
  | LabelNotConsistentError Span LabelName Type
  -- | Type has no label
  | NoLabelError Span LabelName Type
  -- | Constructor has no label
  | NoCtorLabelError Span LabelName CtorName
  -- | Invalid labels from constructor
  | InvalidLabelsError Span CtorName
  -- | Invalid match
  | InvalidMatchError Span Valid Type
  -- | No open datatype in context
  | NoOpenDataError Span
  -- | No datatype in context
  | NoDataError Span
  -- | Types of cases do not match
  | MatchTypesError Span
  -- | Dynamic runtime error
  | TransitivityError Type Type
  -- | Runtime Match error
  | RuntimeMatchError
  -- | Runtime Access error
  | RuntimeAccessError
  -- | Duplicated data declaration
  | DuplicatedDataError Span DataName
  -- | Duplicated ctor declaration
  | DuplicatedCtorError
  -- | Duplicated labels in ctor declaration
  | DuplicatedLabelError CtorName
  -- | Imposible
  | ImposibleError
  deriving (Eq, Show)

data ErrorLevel = PError | Error | Warning
  deriving (Show)

class ErrorInfo a where
  errorLvl   :: a -> ErrorLevel
  errorTitle :: a -> Text

instance ErrorInfo Error where
  errorLvl _ = Error
  errorTitle _ = "Type error!"

instance IsError Error where
  errCtorNotFound       = CtorNotFoundError
  errDataNotFound       = DataNotFoundError
  errConsistency        = ConsistencyError
  errLabelNotConsistent = LabelNotConsistentError
  errNoLabel            = NoLabelError
  errNoCtorLabel        = NoCtorLabelError
  errInvalidLabels      = InvalidLabelsError
  errInvalidMatch       = InvalidMatchError
  errDuplicatedData     = \d -> DuplicatedDataError (span d) d
  errDuplicatedCtor     = DuplicatedCtorError
  errDuplicatedLabels   = DuplicatedLabelError
  errImposible          = ImposibleError

instance HasSpan Error where
  span _ = mempty
