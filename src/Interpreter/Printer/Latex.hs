{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Printer.Latex where

import Data.Text hiding (null)
import Interpreter.Type
import Interpreter.Syntax.Common
import Interpreter.Syntax.EvCore
import qualified Interpreter.Syntax.Core as C
import Interpreter.Env (Env(Env))
import Interpreter.Error
import qualified Interpreter.Eval as E
import qualified Interpreter as I
import Interpreter.Parser (ParseError(ParseError))
import qualified Data.Text as T

class PrintableLatex a where
  ppr :: a -> Text

  needParens :: a -> Bool
  needParens _ = False

  maybeParens :: a -> Text
  maybeParens x
    | needParens x = parens $ ppr x
    | otherwise    = ppr x

parens :: Text -> Text
parens x = "(" <> x <> ")"
ascMaybeParens :: PrintableLatex a => a -> Text
ascMaybeParens x
  | needParens x = "\\ascParens" <> braces (ppr x)
  | otherwise    = ppr x

pprType :: Type -> Text
pprType = ppr

pprExpr :: Expr -> Text
pprExpr = ppr

braces :: Text -> Text
braces x = "{" <> x <> "}"

sepBy :: PrintableLatex a => Text -> [a] -> Text
sepBy sep = intercalate sep . fmap ppr

(<+>) :: Text -> Text -> Text
t1 <+> t2 = t1 <> "~" <> t2

instance PrintableLatex Valid where
  ppr Sound    = "\\mathsf{Sound}"
  ppr Exact    = "\\mathsf{Exact}"
  ppr Complete = "\\mathsf{Complete}"

instance PrintableLatex Name where
  ppr (Name _ d) = "\\mathit" <> braces d

instance PrintableLatex CtorName where
  ppr (CtorName _ d) = "\\mathsf" <> braces d

instance PrintableLatex DataName where
  ppr (DataName _ d o) = "\\mathsf" <> braces d <> "_" <> braces (ppr o)

instance PrintableLatex LabelName where
  ppr (LabelName _ d) = "\\mathit" <> braces d

instance PrintableLatex Openess where
  ppr Open   = "O"
  ppr Closed = "X"

instance PrintableLatex TBase where
  ppr TInt    = "\\mathsf{Int}"
  ppr TString = "\\mathsf{String}"

instance PrintableLatex Type where
  ppr (TBase _ t)    = ppr t
  ppr (TArr _ t1 t2) = maybeParens t1 <+> "\\rightarrow " <+> ppr t2
  ppr (TData _ n)    = ppr n
  ppr (TUnkn _)      = "\\mathsf{?}"
  ppr (TUnknData _)  = "\\mathsf{?_D}"
  ppr (TUnclass _)   = "\\mathsf{?_O}"

  needParens TArr {} = True
  needParens _ = False

instance PrintableLatex Lit where
  ppr (LInt n) = pack $ show n
  ppr (LString s) = "\\verb|\"" <> s <> "\"|"

instance PrintableLatex Evidence where
  ppr (Evidence t) = braces $ "\\color{purple} \\varepsilon_{" <> ppr t <> "}"
  needParens (Evidence t) = needParens t

instance PrintableLatex Case where
  ppr (Case p e) = ppr p <+> "\\mapsto " <+> ppr e

instance PrintableLatex Pattern where
  ppr (CtorP _ c xs) = ppr c <+> sepBy "~" xs
  ppr (DefP _) = "\\_"

instance PrintableLatex BinOp where
  ppr Plus  = "+"
  ppr Minus = "-"
  ppr Times = "*"
  ppr Div   = "/"
  ppr Equal = "="

instance PrintableLatex CtorArg where
  ppr (CtorArg l e) = ppr l <> "=" <> ppr e

instance PrintableLatex Expr where
  ppr (Var x) = ppr x
  ppr (Lit l) = ppr l
  ppr (App e1 e2@App{}) = maybeParens e1 <+> parens (ppr e2)
  ppr (App e1 e2) = maybeParens e1 <+> maybeParens e2
  ppr (Lam x t e) = "\\lambda " <> ppr x <> ":" <> ppr t <> "." <> ppr e
  ppr (Asc ev e t) = "\\ascription{" <> ppr ev <> "}{" <> ascMaybeParens e <> "}{" <> ppr t <> "}"
  ppr (Ctor c args)
    | null args = ppr c
    | otherwise = ppr c <+> "\\{" <> sepBy ",~" args <> "\\}"
  ppr (Match e cs)
    = "\\mathsf{match}" <+> ppr e <+> "\\mathsf{with}" <+> braces (sepBy ";~" cs)
  ppr (BinOp bop e1 e2) = maybeParens e1 <+> ppr bop <+> maybeParens e2
  ppr (Access e l _) = maybeParens e <> "." <> ppr l
  ppr (Value ev e t) = ppr (Asc ev e t)
  ppr (Clos x tx e _) = ppr (Lam x tx e)
  ppr ToJson = "\\mathsf{toJSON}"
  ppr FromJson = "\\mathsf{fromJSON}"
  ppr (Highlight e) = "\\boxed" <> braces (ppr e)

  needParens Lam {} = True
  needParens Asc {} = True
  needParens BinOp {} = True
  needParens Value {} = True
  needParens _ = False



-- Core

instance PrintableLatex C.Case where
  ppr (C.Case p e) = ppr p <+> "\\mapsto " <+> ppr e

instance PrintableLatex C.CtorArg where
  ppr (C.CtorArg _ l e) = ppr l <> "=" <> ppr e

instance PrintableLatex C.Expr where
  ppr (C.Var _ x) = ppr x
  ppr (C.Lit _ l) = ppr l
  ppr (C.App _ e1 e2@C.App{}) = maybeParens e1 <+> parens (ppr e2)
  ppr (C.App _ e1 e2) = maybeParens e1 <+> maybeParens e2
  ppr (C.Lam _ x t e) = "\\lambda " <> ppr x <> ":" <> ppr t <> "." <> ppr e
  ppr (C.Asc _ e t) = maybeParens e <+> ":" <+> ppr t
  ppr (C.Ctor _ c args)
    | null args = ppr c
    | otherwise = ppr c <+> "\\{" <> sepBy ",~" args <> "\\}"
  ppr (C.Match _ e cs)
    = "\\mathsf{match}" <+> ppr e <+> "\\mathsf{with}" <+> braces (sepBy ";~" cs)
  ppr (C.BinOp _ bop e1 e2) = maybeParens e1 <+> ppr bop <+> maybeParens e2
  ppr (C.Access _ e l) = maybeParens e <> "." <> ppr l

  needParens C.Lam {} = True
  needParens C.Asc {} = True
  needParens C.BinOp {} = True
  needParens _ = False

instance PrintableLatex (Env a) where
  ppr (Env dataCtx ctorCtx varCtx) =
    let emptyCtx = "\\cdot"
        pprCtx ctx txt = if null ctx then emptyCtx else txt
    in pprCtx dataCtx "\\Delta" <> ";" <> pprCtx ctorCtx "\\Xi" <> ";" <> pprCtx varCtx "\\Gamma"

instance PrintableLatex Error where
  ppr (VarNotFoundError s x)
    = "Variable $" <> ppr x <> "$ is not defined."
  ppr (CtorNotFoundError s c)
    = "Unclassified data constructor $" <> ppr c <> "$ cannot be instantiated positionaly."
  ppr (DataNotFoundError s d)
    = "Datatype $" <> ppr d <> "$ is not defined."
  ppr (ConsistencyError s t1 t2)
    = "Types $" <> ppr t1 <> "$ and $" <> ppr t2 <> "$ are not consistent."
  ppr (LabelNotConsistentError s l t)
    = "Types for label $" <> ppr l <> "$ in type $" <> ppr t <> "$ are not consistent."
  ppr (NoLabelError s l t)
    = "Type $" <> ppr t <> "$ does not have label $" <> ppr l <> "$."
  ppr (NoCtorLabelError s l c)
    = "Constructor $" <> ppr c <> "$ does not have label $" <> ppr l <> "$."
  ppr (InvalidLabelsError s c)
    = "Labels for constructor $" <> ppr c <> "$ do not match the definition."
  ppr (InvalidMatchError s v t)
    = "Invalid match: Patterns are not $" <> ppr v <> "$ with respect to $" <> ppr t <> "$."
  ppr (DuplicatedDataError s d)
    = "Duplicated datatype definition: There two datatypes with the name $" <> ppr d <> "$."
  ppr DuplicatedCtorError
    = "Duplicated constructor definitions."
  ppr (DuplicatedLabelError c)
    = "Duplicated labels in constructor definition: Constructor $"<> ppr c <> "$ have duplicated labels."
  ppr (NoOpenDataError s) = "Unclassified data is used, but no open datatype is defined."
  ppr (NoDataError s) = "A constructor is applied, but no datatype is defined."
  ppr (MatchTypesError s) = "The types of the branches."
  ppr ImposibleError = "Imposible!"
  ppr _ = ppr ImposibleError

instance PrintableLatex ErrorLevel where
  ppr PError  = "Parse error"
  ppr Error   = "Error!"
  ppr Warning = "Warning"

instance PrintableLatex E.Error where
  ppr (E.EMatch c) = "No case matches constructor $" <> ppr c <> "$ in match expression."
  ppr (E.EAccess c l) = "Constructor $" <> ppr c <> "$ doesn't have label $" <> ppr l <> "$."
  ppr (E.ETrans t1 t2) =
    "Consistent transitivity between $" <> ppr t1 <> "$ and $" <> ppr t2 <> "$ is not defined."
  ppr (E.EVar n) = "Variable $" <> ppr n <> "$ is not in scope."
  ppr (E.EToJSON e) = "Cannot be converted to JSON: " <> ppr e
  ppr (E.EFromJSON txt) = "Cannot be converted from JSON: '" <> txt <> "'."

instance PrintableLatex I.Error where
  ppr I.NoExprWarning = "The program typechecked correctly, but there is no expression to evaluate."

instance PrintableLatex I.OutError where
  ppr (I.EE e) = ppr e
  ppr (I.IE e) = ppr e
  ppr (I.TE e) = ppr e
  ppr (I.PE (ParseError s)) = T.pack s
