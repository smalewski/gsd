{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Printer where

import Data.Text hiding (null)
import Interpreter.Type
import Interpreter.Syntax.Common
import Interpreter.Syntax.EvCore
import qualified Interpreter.Syntax.Core as C
import Interpreter.Env (Env(Env))
import Text.Pandoc (readLaTeX, writeHtml5String, ReaderOptions(..), WriterOptions(..), TrackChanges (AcceptChanges), WrapOption (WrapNone), ObfuscationMethod (NoObfuscation), CiteMethod (Natbib), TopLevelDivision (TopLevelSection), ReferenceLocation (EndOfBlock), HTMLMathMethod (MathML), runPure)
import Data.Either (fromRight)
import Control.Monad ((<=<))

preRender :: Text -> Text
preRender x = preRender' $ "$" <> x <> "$"

preRender' :: Text -> Text
preRender' = fromRight mempty . runPure . (writeHtml5String wopts <=< readLaTeX ropts)
  where
    ropts :: ReaderOptions
    ropts = ReaderOptions { readerExtensions            = mempty
                          , readerStandalone            = False
                          , readerColumns               = 1000
                          , readerTabStop               = 1000
                          , readerIndentedCodeClasses   = []
                          , readerAbbreviations         = mempty
                          , readerDefaultImageExtension = mempty
                          , readerTrackChanges          = AcceptChanges
                          , readerStripComments         = False
                          }
    wopts :: WriterOptions
    wopts = WriterOptions { writerTemplate          = Nothing
                          , writerVariables         = mempty
                          , writerTabStop           = 1000
                          , writerTableOfContents   = False
                          , writerIncremental       = False
                          , writerHTMLMathMethod    = MathML
                          , writerNumberSections    = False
                          , writerNumberOffset      = []
                          , writerSectionDivs       = False
                          , writerExtensions        = mempty
                          , writerReferenceLinks    = False
                          , writerDpi               = 10
                          , writerWrapText          = WrapNone
                          , writerColumns           = 1000
                          , writerEmailObfuscation  = NoObfuscation
                          , writerIdentifierPrefix  = "gsd"
                          , writerCiteMethod        = Natbib
                          , writerHtmlQTags         = False
                          , writerSlideLevel        = Nothing
                          , writerTopLevelDivision  = TopLevelSection
                          , writerListings          = False
                          , writerHighlightStyle    = Nothing
                          , writerSetextHeaders     = False
                          , writerEpubSubdirectory  = ""
                          , writerEpubMetadata      = Nothing
                          , writerEpubFonts         = mempty
                          , writerEpubChapterLevel  = 0
                          , writerTOCDepth          = 0
                          , writerReferenceDoc      = Nothing
                          , writerReferenceLocation = EndOfBlock
                          , writerSyntaxMap         = mempty
                          , writerPreferAscii       = False
                          }

class Printable a where
  ppr :: a -> Text

  needParens :: a -> Bool
  needParens _ = False

  maybeParens :: a -> Text
  maybeParens x
    | needParens x = parens $ ppr x
    | otherwise    = ppr x

(<+>) :: Text -> Text -> Text
t1 <+> t2 = t1 <> "~" <> t2

parens :: Text -> Text
parens x = "(" <> x <> ")"

ascMaybeParens :: Printable a => a -> Text
ascMaybeParens x
  | needParens x = "\\ascParens" <> braces (ppr x)
  | otherwise    = ppr x

braces :: Text -> Text
braces x = "{" <> x <> "}"

sepBy :: Printable a => Text -> [a] -> Text
sepBy sep = intercalate sep . fmap ppr

instance Printable Valid where
  ppr Sound    = "\\mathsf{Sound}"
  ppr Exact    = "\\mathsf{Exact}"
  ppr Complete = "\\mathsf{Complete}"

instance Printable Name where
  ppr (Name _ d) = "\\mathit" <> braces d

instance Printable CtorName where
  ppr (CtorName _ d) = "\\mathsf" <> braces d

instance Printable DataName where
  ppr (DataName _ d) = "\\mathsf" <> braces d

instance Printable LabelName where
  ppr (LabelName _ d) = "\\mathit" <> braces d

instance Printable TBase where
  ppr TInt    = "\\mathsf{Int}"
  ppr TString = "\\mathsf{String}"

instance Printable Type where
  ppr (TBase _ t)    = ppr t
  ppr (TArr _ t1 t2) = maybeParens t1 <+> "\\rightarrow " <+> ppr t2
  ppr (TData _ n)    = ppr n
  ppr (TUnkn _)      = "\\mathsf{?}"
  ppr (TUnknData _)  = "\\mathsf{?_D}"
  ppr (TUnclass _)   = "\\mathsf{?_O}"

  needParens TArr {} = True
  needParens _ = False

instance Printable Lit where
  ppr (LInt n) = pack $ show n
  ppr (LString s) = "\\verb|\"" <> s <> "\"|"

instance Printable Evidence where
  ppr (Evidence t) = braces $ "\\color{purple} \\varepsilon_{" <> ppr t <> "}"
  needParens (Evidence t) = needParens t

instance Printable Case where
  ppr (Case p e) = ppr p <+> "\\mapsto " <+> ppr e

instance Printable Pattern where
  ppr (CtorP _ c xs) = ppr c <+> sepBy "~" xs
  ppr (DefP _) = "\\_"

instance Printable BinOp where
  ppr Plus  = "+"
  ppr Minus = "-"
  ppr Times = "*"
  ppr Div   = "/"
  ppr Equal = "="

instance Printable CtorArg where
  ppr (CtorArg l e) = ppr l <> "=" <> ppr e

instance Printable Expr where
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

instance Printable C.Case where
  ppr (C.Case p e) = ppr p <+> "\\mapsto " <+> ppr e

instance Printable C.CtorArg where
  ppr (C.CtorArg _ l e) = ppr l <> "=" <> ppr e

instance Printable C.Expr where
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

instance Printable (Env a) where
  ppr (Env dataCtx ctorCtx varCtx) =
    let emptyCtx = "\\cdot"
        pprCtx ctx txt = if null ctx then emptyCtx else txt
    in pprCtx dataCtx "\\Delta" <> ";" <> pprCtx ctorCtx "\\Xi" <> ";" <> pprCtx varCtx "\\Gamma"
