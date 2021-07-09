{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Printer where

import Data.Text

class Printable a where
  ppr :: a -> Text

  needParens :: a -> Bool
  needParens _ = False

  maybeParens :: a -> Text
  maybeParens x
    | needParens x = parens $ ppr x
    | otherwise    = ppr x

parens :: Text -> Text
parens x = "(" <> x <> ")"
