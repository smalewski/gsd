{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Interpreter.Printer where

import Data.Text (Text)
import Data.Data (Data)
import qualified Interpreter.Printer.Latex as Latex
import Interpreter.Printer.Latex (PrintableLatex)
import qualified Interpreter.Printer.Plain as Plain
import Interpreter.Printer.Plain (PrintablePlain)

data Format = Latex | Plain
  deriving (Eq, Show, Data)

class Printable a where
  pprLatex :: a -> Text
  pprPlain :: a -> Text

  ppr :: Format -> a -> Text
  ppr Latex = pprLatex
  ppr Plain = pprPlain

instance (PrintableLatex a, PrintablePlain a) => Printable a where
  pprLatex = Latex.ppr
  pprPlain = Plain.ppr
