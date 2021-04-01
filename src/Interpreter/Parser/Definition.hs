{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Parser.Definition where

import Control.Monad (void, when)
import Control.Monad.Combinators.Expr
import Interpreter.Env (CtorInfo (CtorInfo, argTypes), DataInfo (DataInfo), Env(Env), lookupVar', lookupCtor', emptyEnv)
import Interpreter.Parser.Definition.Data (dataDefinitionP)
import Interpreter.Parser.Definition.Function
import Interpreter.Parser.Helpers
import qualified Interpreter.Parser.Pattern as Pattern
import qualified Interpreter.Parser.Type as Type
import Interpreter.Span
import Interpreter.Syntax.Base
import Interpreter.Syntax.Common
import Interpreter.Type
import Text.Megaparsec hiding (Label)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (span)
import Data.List (partition, sortOn, sort, foldl')
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Map.Strict (singleton, fromList)
import qualified Interpreter.Parser.Expr as Expr
import Text.Megaparsec.Char (newline)

data Def
  = DataDef (Maybe DataName) DataInfo [(CtorName, CtorInfo)]
  | CtorDef CtorName CtorInfo
  | TypeDef Name Type
  | ConstDef Name Expr
  | FunDef Name [Name] Expr
  | Raw Expr

parser :: Parser [Def]
parser = many (topLevel defP) <* whitespace <* eof
  where
    defP = dataDefP <|> try typeDefP <|> try constDefP <|> try funDefP <|> rawP

dataDefP :: Parser Def
dataDefP = do
  (d, di, cs) <- dataDefinitionP
  pure $ DataDef (Just d) di cs

typeDefP :: Parser Def
typeDefP = uncurry TypeDef <$> typeDeclP

constDefP :: Parser Def
constDefP = uncurry ConstDef <$> constDeclP

funDefP :: Parser Def
funDefP = do
  (name, xs, e) <- funDeclP
  pure $ FunDef name xs e

rawP :: Parser Def
rawP = Raw <$> Expr.parser
