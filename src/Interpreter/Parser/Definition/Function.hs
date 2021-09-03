{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Parser.Definition.Function where

import Control.Monad (void, when)
import Control.Monad.Combinators.Expr
import Interpreter.Parser.Helpers
import qualified Interpreter.Parser.Type as Type
import qualified Interpreter.Parser.Pattern as Pattern
import qualified Interpreter.Parser.Expr as Expr
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

typeDeclP :: Parser (Name, Type)
typeDeclP = do
  name <- try $ do
    name' <- nameP
    void $ txt ":"
    pure name'
  ty   <- Type.parser
  pure (name, ty)

funDeclP :: Parser (Name, [Name], Expr)
funDeclP = do
  (name, xs) <- try $ do
    name' <- nameP
    xs'   <- some nameP
    void $ txt "="
    pure (name', xs')
  maybe pass (customFailure . EFunVars) $ getDuplicate xs
  withLineFold $ do
    e <- Expr.parser
    pure (name, xs, e)

constDeclP :: Parser (Name, Expr)
constDeclP = do
  name <- try $ do
    name' <- nameP
    void $ txt "="
    pure name'
  withLineFold $ do
    e <- Expr.parser
    pure (name, e)
