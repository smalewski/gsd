{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Parser.Definition where

import Interpreter.Env (CtorInfo, DataInfo)
import Interpreter.Parser.Definition.Data (dataDefinitionP)
import Interpreter.Parser.Definition.Function
import Interpreter.Parser.Helpers
import Interpreter.Syntax.Base
import Interpreter.Syntax.Common
import Interpreter.Type
import Text.Megaparsec hiding (Label)
import Prelude hiding (span)
import qualified Interpreter.Parser.Expr as Expr

data Def
  = DataDef DataName DataInfo [(CtorName, CtorInfo)]
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
  pure $ DataDef d di cs

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
