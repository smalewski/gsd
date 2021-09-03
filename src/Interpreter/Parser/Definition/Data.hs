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
import qualified Data.Set as S

dataDefinitionP :: Parser (DataName, DataInfo, [(CtorName, CtorInfo)])
dataDefinitionP = do
  i <- getOffset
  region (handleError i) $ do
    o <- try $ do
      o' <- withDefault Closed openessP
      void $ keyword "data"
      pure o'
    d  <- lexeme dataNameP <?> "datatype identifier"
    dataCtorsP d o <|> emptyDataP d o
  where
    dataCtorsP d o = do
      void . lexeme' $ char '='
      let sep = whitespace' *> lexeme' (char '|')
      cs <- sepBy ctorDefinitionP sep <?> "constructor definition"
      let cnames = fst <$> cs
      when (S.size (S.fromList cnames) /= length cnames) (customFailure $ EDupCtor d)
      pure (setOpeness o d, DataInfo cnames, cs)

    emptyDataP d o = pure (setOpeness o d, DataInfo [], [])

    handleError _ e@TrivialError{} = e
    handleError i e@(FancyError _ _) = setErrorOffset i e

ctorDefinitionP :: Parser (CtorName, CtorInfo)
ctorDefinitionP = label "constructor definition" $ do
  c <- ctorNameP
  argsP c <|> emptyArgsP c
  where
    argsP c = label argsLabel $ do
    --  whitespace'
      void . lexeme' $ char '{'
      args <- sepBy argDefinitionP' (lexeme' $ char ',')
      void $ char '}'
      pure (c, CtorInfo args)
    emptyArgsP c = pure (c, CtorInfo [])
    argsLabel = "field definitions (<label> : <type> , ... , <label> : <type>)"
    argDefinitionP' = argDefinitionP <?> "field definition (<label> : <type>)"

argDefinitionP :: Parser (LabelName, Type)
argDefinitionP =
  sameLine $ do
    l <- lexeme' labelNameP
    void . lexeme' $  char ':'
    t <- Type.parser
    pure (l, t)

openessP :: Parser Openess
openessP = openP <|> closedP
  where
    openP   = keyword "open" $> Open
    closedP = keyword "closed" $> Closed
