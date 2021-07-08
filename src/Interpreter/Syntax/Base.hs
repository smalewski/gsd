{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Interpreter.Syntax.Base where

import Prelude hiding (span)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Interpreter.Type
import Interpreter.Syntax.Common
import Interpreter.Span

-- | Expressions
data Expr
  -- | Variables
  = Var Span Name
  -- | Constants
  | Lit Span Lit
  -- | Funtion application
  | App Span Expr Expr [Expr]
  -- | Lambdas
  | Lam Span [(Name, Type)] Expr
  -- | Ascriptions
  | Asc Span Expr Type
  -- | Constructor with labels
  | CtorLbl Span CtorName [(Span, LabelName, Expr)]
  -- | Constructor positional
  | CtorPos Span CtorName [Expr]
  -- | Pattern matching
  | Match Span Expr [Case]
  -- | Binary operator
  | BinOp Span BinOp Expr Expr
  -- | Let binding
  | Let Span [LetBinding Expr] Expr
  -- | Field access
  | Access Span Expr LabelName
  -- | If expression
  | Ite Span Expr Expr Expr
  deriving (Eq, Show)

data LetBinding a = LetBinding { lbSpan :: Span, lbName :: Name, lbType :: Type, lbExpr :: a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Cases of a match expression
data Case = Case { casePattern :: Pattern, caseExpr :: Expr }
  deriving (Eq, Show)

instance HasSpan Expr where
  span (Var sp _)       = sp
  span (Lit sp _)       = sp
  span (App sp _ _ _)   = sp
  span (Lam sp _ _)     = sp
  span (Asc sp _ _)     = sp
  span (CtorLbl sp _ _) = sp
  span (CtorPos sp _ _) = sp
  span (Match sp _ _)   = sp
  span (BinOp sp _ _ _) = sp
  span (Let sp _ _)     = sp
  span (Access sp _ _)  = sp
  span (Ite sp _ _ _)   = sp

instance HasSpan Case where
  span (Case pat e) = span pat <> span e

thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

data FoundCtor
  = FCtor CtorName (Either Int [LabelName])
  | FPat CtorName Int
  deriving (Eq)

-- Constructors before patterns
-- Labeled before positional
instance Ord FoundCtor where
  FCtor {} <= FPat {} = True
  FPat {} <= FCtor {} = False
  (FPat c n) <= (FPat c' n') = c < c' || (c == c' && n <= n')
  (FCtor c (Left n))   <= (FCtor c' (Left n'))   = c < c' || (c == c' && n <= n')
  (FCtor c (Left n))   <= (FCtor c' (Right ls')) = c < c'
  (FCtor c (Right ls)) <= (FCtor c' (Left n'))   = c <= c'
  (FCtor c (Right ls)) <= (FCtor c' (Right ls')) = c < c' || (c == c' && length ls <= length ls')

unFoundCtor :: FoundCtor -> (CtorName, Either Int [LabelName])
unFoundCtor (FCtor c s) = (c, s)
unFoundCtor (FPat c n) = (c, Left n)

isFoundCtor :: FoundCtor -> Bool
isFoundCtor FCtor{} = True
isFoundCtor _ = False

findCtors :: Expr -> [FoundCtor]
findCtors (Var _ _) = []
findCtors (Lit _ _) = []
findCtors (App _ e1 e2 es) = concatMap findCtors (e1 : e2 : es)
findCtors (Lam _ _ e) = findCtors e
findCtors (Asc _ e _) = findCtors e
findCtors (CtorLbl _ c es) = (FCtor c . Right $ fmap snd3 es) : concatMap (findCtors . thd3) es
findCtors (CtorPos _ c es) = (FCtor c . Left $ length es) : concatMap findCtors es
findCtors (Match _ e cs) = findCtors e <> concatMap findCtorsCase cs
findCtors (BinOp _ _ e1 e2) = concatMap findCtors [e1, e2]
findCtors (Access _ e _) = findCtors e
findCtors (Ite _ e2 e3 e4) = concatMap findCtors [e2, e3, e4]
findCtors (Let _ bs e) = concatMap (findCtors . lbExpr) bs <> findCtors e

mapCase :: (Expr -> Expr) -> Case -> Case
mapCase f (Case p e) = Case p (f e)

findCtorsCase :: Case -> [FoundCtor]
findCtorsCase (Case p e) = findCtorsPat p <> findCtors e
  where
    findCtorsPat (CtorP _ c xs) = [FPat c $ length xs]
    findCtorsPat _              = []


mapBinding :: (Type -> Type) -> (a -> b) -> LetBinding a -> LetBinding b
mapBinding f g (LetBinding s n t e) = LetBinding s n (f t) (g e)
