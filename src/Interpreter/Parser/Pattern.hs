{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Parser.Pattern (parser) where

import Prelude hiding (span)
import Control.Monad (void)
import Interpreter.Syntax.Common
import Interpreter.Span
import Interpreter.Parser.Helpers
import Text.Megaparsec

parser :: Parser Pattern
parser = sameLine parser' <?> "pattern"
  where
    parser' = ctorPattern <|> defPattern

ctorPattern :: Parser Pattern
ctorPattern = do
  startPos <- getOffset
  ctor <- ctorNameP
  xs <- many nameP
  let sp = span xs
  pure $ CtorP (startPos .> sp) ctor xs

{-
arityPattern :: Parser Pattern
arityPattern = do
  startPos <- getOffset
  void $ char '_'
  name <- nameP
  xs <- many nameP
  let sp = span xs
  pure $ ArityP (startPos .> sp) name xs
-}

defPattern :: Parser Pattern
defPattern = do
  DefP . fst <$> withSpan (char '_')
