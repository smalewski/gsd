module Interpreter.Syntax.EvCore where

import Prelude hiding (span)
import Interpreter.Type
import Interpreter.Syntax.Common
import Data.IORef (IORef)
import Interpreter.Env (Env)

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
isRawValue _           = False
