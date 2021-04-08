{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Stdlib where

import Data.Text (Text)

stdlib :: Text
stdlib =
  "\n\n" <>
  "open data U\n" <>
  "data Bool = False | True\n\n" <>
  "not : Bool -> Bool\n" <>
  "not b = match b with\n" <>
  "          False => True\n" <>
  "          True  => False\n" <>
  "fromJSON : String -> ?D\n" <>
  "toJSON : ?D -> String\n"
