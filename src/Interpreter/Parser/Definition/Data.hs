{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Parser.Definition.Data where

import Control.Monad (void, when)
import Control.Monad.Combinators.Expr
import Interpreter.Parser.Helpers
import qualified Interpreter.Parser.Type as Type
import qualified Interpreter.Parser.Pattern as Pattern
import Prelude hiding (span)
import Interpreter.Span
import Interpreter.Type

import Text.Megaparsec hiding (Label)
import qualified Text.Megaparsec.Char.Lexer as L

import Interpreter.Syntax.Base
import Interpreter.Syntax.Common
import Interpreter.Env
import qualified Data.Map.Strict as Map
import Data.Bifunctor (first)
import Data.Functor (($>))
import Text.Megaparsec.Char (newline)

dataDefinitionP :: Parser (DataName, DataInfo, [(CtorName, CtorInfo)])
dataDefinitionP = withLineFold $ do
  o  <- withDefault Closed openessP
  void $ keyword "data"
  d  <- lexeme dataNameP
  try (dataCtorsP d o) <|> emptyDataP d o
  where
    dataCtorsP d o = do
      void . lexeme' $ char '='
      let sep = try (whitespace' *> lexeme' (char '|'))
      cs <- sepBy ctorDefinitionP sep
      let cnames = fst <$> cs
      pure (d, DataInfo o cnames, cs)

    emptyDataP d o = pure (d, DataInfo o [], [])

ctorDefinitionP :: Parser (CtorName, CtorInfo)
ctorDefinitionP = do
  c <- ctorNameP
  try (argsP c) <|> emptyArgsP c
  where
    argsP c = do
      whitespace'
      void . lexeme' $ char '{'
      args <- sepBy argDefinitionP (lexeme' $ char ',')
      void $ char '}'
      pure (c, CtorInfo args)

    emptyArgsP c = pure (c, CtorInfo [])

argDefinitionP :: Parser (LabelName, Type)
argDefinitionP = sameLine $ do
  l <- lexeme' labelNameP
  void . lexeme' $  char ':'
  t <- Type.parser
  pure (l, t)

openessP :: Parser Openess
openessP = openP <|> closedP
  where
    openP   = keyword "open" $> Open
    closedP = keyword "closed" $> Closed
