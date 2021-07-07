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

isUnkn :: Type -> Bool
isUnkn TUnkn{}     = True
isUnkn TUnknData{} = True
isUnkn TUnclass{}  = True
isUnkn _           = False

litType :: Span -> Lit -> Type
litType sp (LInt _) = TBase sp TInt
litType sp (LString _) = TBase sp TString

binOpType :: BinOp -> (Type, Type, Type)
binOpType Equal =
  let boolTy = TData mempty (DataName mempty "Bool" Closed)
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


isOpen :: DataName -> Bool
isOpen (DataName _ _ o) = o == Open

precise :: Type -> Type -> Bool
precise t1 t2 | t1 == t2 = True
precise _            (TUnkn _)     = True
precise (TUnclass _) (TUnknData _) = True
precise (TData _ _)  (TUnknData _) = True
precise (TData _ d)  (TUnclass _)  = isOpen d
precise _ _ = False

meet :: Type -> Type -> Maybe Type
meet (TArr sp1 t11 t12) (TArr sp2 t21 t22) =
  TArr (sp1 <> sp2) <$> meet t11 t21 <*> meet t12 t22
meet t1 t2
  | precise t1 t2 = Just t1
  | precise t2 t1 = Just t2
  | otherwise = Nothing

meetList :: [Type] -> Maybe Type
meetList [] = Nothing
meetList [t] = Just t
meetList (t:t':ts) =
  case meet t t' of
    Nothing -> Nothing
    Just tt -> meetList (tt : ts)

join :: Type -> Type -> Type
join (TArr sp1 t11 t12) (TArr sp2 t21 t22) =
  TArr (sp1 <> sp2) (join t11 t21) (join t12 t22)
join d1@(TData _ n1) d2@(TData _ n2)
  | d1 == d2               = d1
  | isOpen n1 && isOpen n2 = TUnclass mempty
  | otherwise              = TUnknData mempty
join t1 t2
  | precise t1 t2 = t2
  | precise t2 t1 = t1
  | otherwise = TUnkn mempty

joinList :: [Type] -> Type
joinList []  = TUnkn mempty
joinList (t:ts) = foldr join t ts

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
