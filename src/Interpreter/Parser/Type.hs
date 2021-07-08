{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Parser.Type (parser) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Interpreter.Parser.Helpers
import Interpreter.Span
import Interpreter.Type
import Text.Megaparsec
import Prelude hiding (span)

parser :: Parser Type
parser = makeExprParser (lexeme tyP) tyOperators <?> "type"

tyOperators :: [[Operator Parser Type]]
tyOperators =
  [[InfixR (lexeme' arrOp)]]
  where
    arrOp = do
      void $ txt "->"
      pure $ \e1 e2 ->
        let sp = span e1 <> span e2
         in TArr sp e1 e2

tyP :: Parser Type
tyP =
  betweenParens parser
    <|> baseP
    <|> dataP
    <|> unknDataP
    <|> unclassP
    <|> unknP

baseP :: Parser Type
baseP = uncurry TBase <$> withSpan (intP <|> stringP)
  where
    intP = TInt <$ txt "Int"
    stringP = TString <$ txt "String"

dataP :: Parser Type
dataP = uncurry TData <$> withSpan dataNameP

unknP :: Parser Type
unknP = TUnkn . fst <$> withSpan (txt "?")

unknDataP :: Parser Type
unknDataP = TUnknData . fst <$> withSpan (txt "?D")

unclassP :: Parser Type
unclassP = TUnclass . fst <$> withSpan (txt "?U")
