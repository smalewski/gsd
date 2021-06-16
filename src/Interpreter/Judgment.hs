{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Judgment where

import Interpreter.Printer
import Interpreter.Syntax.Core
import Data.Text (Text)
import Interpreter.Env
import Interpreter.Type
import Data.Maybe (fromMaybe)

class Printable a => Judgment a where
  premises :: Env Type -> a -> [Text]
  judgment :: Env Type -> a -> Type -> (Type, Text)

  pprJ :: a -> Text
  pprJ = ppr

  pprTree :: a -> Text
  pprTree x = "\\begin{prooftree}" <> ppr x <> "\\end{prooftree}"

domF :: Type -> Text
domF t = "\\widetilde{dom_\\Delta}(" <> ppr t <> ")"

conJ :: Text -> Text -> Text
conJ t1 t2 = "\\Delta \vdash" <> t1 <> "\\sim" <> t2

axP :: Text -> Text
axP j = "\\AXC{" <>  j <> "}"
unP :: Text -> Text -> Text
unP j p1 =  p1 <> "\\UIC{" <>  j <> "}"
binP :: Text -> Text -> Text -> Text
binP j p1 p2 =  p1 <>  p2 <> "\\BIC{" <>  j <> "}"
triP :: Text -> Text -> Text -> Text -> Text
triP j p1 p2 p3 =  p1 <>  p2 <>  p3 <> "\\TIC{" <>  j <> "}"
quaP :: Text -> Text -> Text -> Text -> Text -> Text
quaP j p1 p2 p3 p4 =  p1 <>  p2 <>  p3 <>  p4 <> "\\QuaternaryInfC{" <>  j <> "}"
quiP :: Text -> Text -> Text -> Text -> Text -> Text -> Text
quiP j p1 p2 p3 p4 p5 =  p1 <>  p2 <>  p3 <>  p4 <>  p5 <> "\\QuinaryInfC{" <>  j <> "}"

tyJ :: Text -> Expr -> Type -> Text
tyJ ctx e t = "\\Delta,\\Xi,\\Gamma" <> ctx <> "\\vdash" <> ppr e <> "~:~" <> ppr t

mkJ :: Text -> [Text] -> Text
mkJ j [] = axP j
mkJ j [p] = unP j p
mkJ j [p1,p2] = binP j p1 p2
mkJ j [p1,p2,p3] = triP j p1 p2 p3
mkJ j [p1,p2,p3,p4] = quaP j p1 p2 p3 p4
mkJ j [p1,p2,p3,p4,p5] = quiP j p1 p2 p3 p4 p5
mkJ j _ = "too many"

{-
instance Judgment Expr where
  premises env (Var _ x) =
    let t = fromMaybe (TUnkn mempty) $ lookupVar' x env
    in [ "\\Delta; \\Xi \\vdash \\Gamma"
       , ppr t <> "\\doteq \\Gamma(" <> ppr x <> ")"
       ]
  premises env (Lit _ l) = []
  premises env (App _ e1 e2) =
    let (t1, j1) = judgment env e1
        (t2, j2) = judgment env e2
        con = conJ (ppr t2) (domF t1)
    in [ j1, j2, con ]
  premises env (Lam _ x t e) = []
  premises env _ = []

  judgment env e t = (TUnkn mempty, mkJ (tyJ "" e t) (premises env e))
-}
