{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleInstances #-}
module Interpreter.Error where

import Prelude hiding (span)
import Interpreter.Span
import Interpreter.Syntax.Common
import Interpreter.Type
import Data.Text (Text, pack)
import Interpreter.Printer (ppr)

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

errTxt :: ErrorTxt a => a -> (Text, Text)
errTxt x
  = let (ms, title, msg) = errorTxt x
      --  s = maybe "" (\s -> showSpan s <> ": ") ms
        s = ""
    in (title, "\\text{" <> s <> msg <> "}")

class ErrorTxt a where
  errorTxt :: a -> (Maybe Span, Text, Text)

instance ErrorTxt (Maybe Span, Text, Text) where
  errorTxt = id

instance ErrorTxt Error where

  errorTxt (VarNotFoundError s x)
    = (Just s, tErr, "Variable $" <> ppr x <> "$ is not defined.")
  errorTxt (CtorNotFoundError s c)
    = (Just s, tErr, "Unclassified data constructor $" <> ppr c <> "$ cannot be instantiated positionaly.")
  errorTxt (DataNotFoundError s d)
    = (Just s, tErr, "Datatype $" <> ppr d <> "$ is not defined.")
  errorTxt (ConsistencyError s t1 t2)
    = (Just s, tErr, "Types $" <> ppr t1 <> "$ and $" <> ppr t2 <> "$ are not consistent.")
  errorTxt (LabelNotConsistentError s l t)
    = (Just s, tErr, "Types for label $" <> ppr l <> "$ in type $" <> ppr t <> "$ are not consistent.")
  errorTxt (NoLabelError s l t)
    = (Just s, tErr, "Type $" <> ppr t <> "$ does not have label $" <> ppr l <> "$.")
  errorTxt (NoCtorLabelError s l c)
    = (Just s, tErr, "Constructor $" <> ppr c <> "$ does not have label $" <> ppr l <> "$.")
  errorTxt (InvalidLabelsError s c)
    = (Just s, tErr, "Labels for constructor $" <> ppr c <> "$ do not match the definition.")
  errorTxt (InvalidMatchError s v t)
    = (Just s, tErr, "Invalid match: Patterns are not $" <> ppr v <> "$ with respect to $" <> ppr t <> "$.")
  errorTxt (DuplicatedDataError s d)
    = (Just s, tErr, "Duplicated datatype definition: There two datatypes with the name $" <> ppr d <> "$.")
  errorTxt DuplicatedCtorError
    = (Just mempty, tErr, "Duplicated constructor definitions.")
  errorTxt (DuplicatedLabelError c)
    = (Just $ span c, tErr, "Duplicated labels in constructor definition: Constructor $"<> ppr c <> "$ have duplicated labels.")
  errorTxt (NoOpenDataError s) = (Just s, tErr, "Unclassified data is used, but no open datatype is defined.")
  errorTxt (NoDataError s) = (Just s, tErr, "A constructor is applied, but no datatype is defined.")
  errorTxt (MatchTypesError s) = (Just s, tErr, "The types of the branches.")
  errorTxt ImposibleError = (Nothing, "Imposible", "Imposible")

tErr = "Type Error"

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
