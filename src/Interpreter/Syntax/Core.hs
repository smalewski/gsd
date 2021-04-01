module Interpreter.Syntax.Core where

import Prelude hiding (span)
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
  | App Span Expr Expr
  -- | Lambdas
  | Lam Span Name Type Expr
  -- | Ascriptions
  | Asc Span Expr Type
  -- | Constructor
  | Ctor Span CtorName [CtorArg]
  -- | Pattern matching
  | Match Span Expr [Case]
  -- | Binary operator
  | BinOp Span BinOp Expr Expr
  -- | Field access
  | Access Span Expr LabelName
  deriving (Eq, Show)

-- | Cases of a match expression
data Case = Case { casePattern :: Pattern, caseExpr :: Expr }
  deriving (Eq, Show)

data CtorArg = CtorArg { ctorArgSpan :: Span
                       , ctorArgLabel :: LabelName
                       , ctorArgExpr :: Expr
                       } deriving (Eq, Show)

unfoldArgs :: [CtorArg] -> ([Span], [LabelName], [Expr])
unfoldArgs [] = ([], [], [])
unfoldArgs (CtorArg sp l e : cargs) =
  let (sps, ls, es) = unfoldArgs cargs
  in  (sp:sps, l:ls, e:es)

instance HasSpan Expr where
  span (Var sp _)       = sp
  span (Lit sp _)       = sp
  span (App sp _ _)     = sp
  span (Lam sp _ _ _)   = sp
  span (Asc sp _ _)     = sp
  span (Ctor sp _ _)    = sp
  span (Match sp _ _)   = sp
  span (BinOp sp _ _ _) = sp
  span (Access sp _ _)  = sp

instance HasSpan Case where
  span (Case pat e) = span pat <> span e

instance HasSpan CtorArg where
  span (CtorArg sp _ _) = sp

isValue :: Expr -> Bool
isValue Lit{}         = True
isValue Lam{}         = True
isValue (Ctor _ _ xs) = all isValue . fmap ctorArgExpr $ xs
isValue _             = False
