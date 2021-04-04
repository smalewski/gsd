{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Type where

import Data.Text (Text, unpack)
import Interpreter.Span
import Interpreter.Syntax.Common

-- | Gradual types
data Type
  = -- | Base types
    TBase Span TBase
  | -- | Arrow type
    TArr Span Type Type
  | -- | Datatypes
    TData Span DataName
  | -- | Unknown type ?
    TUnkn Span
  | -- | Unknown Datatypes ?D
    TUnknData Span
  | -- | Unclassified data ?U
    TUnclass Span
  deriving (Ord)

instance Show Type where
  show (TBase s t2)   = show t2
  show (TArr s t2 t3) = show t2 <> " -> " <> show t3
  show (TData s d)    = show d
  show (TUnkn s)      = "?"
  show (TUnknData s)  = "?D"
  show (TUnclass s)   = "?U"

-- | Base types
data TBase = TInt | TString
  deriving (Eq, Ord, Show)

data DataName = DataName Span Text

instance Show DataName where
  show (DataName _ x) = unpack x

instance Eq DataName where
  (DataName _ x) == (DataName _ y) = x == y

instance Ord DataName where
  compare (DataName _ x) (DataName _ y) = compare x y

instance HasSpan DataName where
  span (DataName s _) = s

instance Eq Type where
  (TBase _ t1) == (TBase _ t2) = t1 == t2
  (TArr _ t11 t12) == (TArr _ t21 t22) = t11 == t21 && t12 == t22
  (TData _ d1) == (TData _ d2) = d1 == d2
  (TUnkn _) == (TUnkn _) = True
  (TUnknData _) == (TUnknData _) = True
  (TUnclass _) == (TUnclass _) = True
  _ == _ = False

isData :: Type -> Bool
isData TData {} = True
isData TUnkn {} = True
isData TUnknData {} = True
isData TUnclass {} = True
isData _ = False

litType :: Span -> Lit -> Type
litType sp (LInt _) = TBase sp TInt
litType sp (LString _) = TBase sp TString

binOpType :: BinOp -> (Type, Type, Type)
binOpType Equal =
  let boolTy = TData mempty $ DataName mempty "Bool"
      unknTy = TUnkn mempty
  in  (unknTy, unknTy, boolTy)
binOpType _ =
  let intTy = TBase mempty TInt
  in (intTy, intTy, intTy)

instance HasSpan Type where
  span (TBase sp _) = sp
  span (TArr sp _ _) = sp
  span (TData sp _) = sp
  span (TUnkn sp) = sp
  span (TUnknData sp) = sp
  span (TUnclass sp) = sp

-- | Partial functions on Types

-- | Domain
dom' :: Type -> Maybe Type
dom' (TArr _ t _) = Just t
dom' t@TUnkn {} = Just t
dom' _ = Nothing

-- | Codomain
cod' :: Type -> Maybe Type
cod' (TArr _ _ t) = Just t
cod' t@TUnkn {} = Just t
cod' _ = Nothing
