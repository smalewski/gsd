{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Parser.Expr where

import Control.Monad (void, when)
import Control.Monad.Combinators.Expr
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

parser :: Parser Expr
parser = (try ascP <|> expr) <?> "expression"

exprOperators :: [[Operator Parser Expr]]
exprOperators =
  [ [infixL timesP, infixL divP],
    [infixL plusP, infixL minusP],
    [infixL equalP]
  ]
  where
    infixL opP = InfixL (lexeme' $ binOp opP)
    binOp opP = do
      op <- opP
      pure $ \e1 e2 ->
        let sp = span e1 <> span e2
         in BinOp sp op e1 e2
    plusP = Plus <$ lexeme' (chunk "+")
    minusP = Minus <$ lexeme' (chunk "-")
    timesP = Times <$ lexeme' (chunk "*")
    divP = Div <$ lexeme' (chunk "/")
    equalP = Equal <$ lexeme' (chunk "==")

expr :: Parser Expr
expr = makeExprParser term exprOperators

term :: Parser Expr
term = lexeme term' <?> "expression"
  where
    term' = ifP
        <|> matchP
        <|> lamP
        <|> letP
        <|> try applyOrCtorPosP
        <|> baseExprP

baseExprP :: Parser Expr
baseExprP = try accessP <|> accessTerm

accessTerm :: Parser Expr
accessTerm = litP
          <|> varP
          <|> try ctorP
          <|> ctor0P
          <|> betweenParens parser

litP :: Parser Expr
litP = uncurry Lit <$> withSpan litP'
  where
    litP' = intP <|> stringP <?> "literal"
    intP = LInt <$> (try signedIntP <|> unsignedIntP)
    unsignedIntP = lexeme L.decimal
    signedIntP = betweenParens $ L.signed whitespace L.decimal
    stringP = LString <$> stringLiteral
    stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

ascP :: Parser Expr
ascP = sameLine $ do
  startPos <- getOffset
  body <- expr
  void $ txt ":"
  ty <- Type.parser
  pure $ Asc (startPos .> span ty) body ty

ifP :: Parser Expr
ifP = withLineFold $ do
  startPos <- getOffset
  keyword "if"
  cond <- lexeme' parser
  keyword "then"
  trueCase <- lexeme' parser
  keyword "else"
  falseCase <- parser
  pure $ Ite (startPos .> span falseCase) cond trueCase falseCase

matchP :: Parser Expr
matchP = do
  startPos <- getOffset
  keyword "match"
  e <- lexeme' parser
  keyword "with"
  indentation <- L.indentLevel
  let caseP' = withIndent indentation caseP <?> "match case"
  cases <- some caseP'
  notFollowedBy caseP <?> "properly indented match case"
  pure $ Match (startPos .> span cases) e cases
  where
    caseP = withLineFold $ do
      pat <- lexeme' Pattern.parser <|> fail "Invalid pattern."
      void . lexeme' $ chunk "=>"
      Case pat <$> parser

lamP :: Parser Expr
lamP = withLineFold $ do
  startPos <- getOffset
  args <- lexeme' lambdaHead
  body <- parser
  pure $ Lam (startPos .> span body) args body
  where
    lambdaHead = sameLine $ do
      void . lexeme $ char '\\'
      args <- argsP
      void . lexeme $ (chunk "=>" <?> "arrow")
      pure args
    argsP = try typedArgsP <|> untypedArgsP
    untypedArgsP = do
      xs <- some $ lexeme nameP
      pure . zip xs . repeat $ TUnkn mempty
    typedArgsP = do
      xs <- some $ lexeme nameP
      void . lexeme $ chunk ":"
      ts <- some $ lexeme Type.parser
      when (length xs /= length ts) $ fail "different amount of variables and types"
      pure $ zip xs ts

varP :: Parser Expr
varP = uncurry Var <$> withSpan nameP

letP :: Parser Expr
letP = do
  (startPos, bindings) <- withLineFold $ do
    startPos <- getOffset
    keyword "let"
    indentation <- L.indentLevel
    let declP' = withIndent indentation declP <?> "let binding"
    decls <- lexeme' declP' `sepBy1` whitespace'
    pure (startPos, decls)
  e <- withLineFold $ (keyword "in" <?> inLabel) *> parser
  pure $ Let (startPos .> span e) bindings e
  where
    inLabel = "properly indented let binding or 'in' keyword"
    declP = withLineFold $ do
      startPos <- getOffset
      x <- nameP
      t <- option (TUnkn mempty) (txt ":" *> Type.parser)
      void . lexeme' $ char '='
      body <- parser
      pure $ LetBinding (startPos .> span body) x t body

ctorP :: Parser Expr
ctorP = do
  startPos <- getOffset
  ctorName <- ctorNameP
  void . lexeme' $ char '{'
  args <- lexeme argP `sepBy` lexeme' (char ',')
  void $ char '}'
  pure $ CtorLbl (startPos .> span args) ctorName args
  where
    argP = do
      startPos <- getOffset
      x <- labelNameP
      void . lexeme' $ char '='
      e <- parser
      pure (startPos .> span e, x, e)

ctor0P :: Parser Expr
ctor0P = do
  startPos <- getOffset
  ctorName <- ctorNameP
  endPos <- getOffset
  pure $ CtorLbl (Span startPos endPos) ctorName mempty

applyOrCtorPosP :: Parser Expr
applyOrCtorPosP = sameLine (ctorPosP <|> applyP)

ctorPosP :: Parser Expr
ctorPosP = sameLine $ do
  startPos <- getOffset
  ctorName <- ctorNameP
  args <- some $ lexeme baseExprP
  pure $ CtorPos (startPos .> span args) ctorName args

applyP :: Parser Expr
applyP = sameLine $ do
  startPos <- getOffset
  e1 <- lexeme baseExprP
  e2 <- lexeme baseExprP
  es <- many $ lexeme baseExprP
  pure $ App (startPos .> span es) e1 e2 es

accessP :: Parser Expr
accessP = do
  startPos <- getOffset
  e <- accessTerm
  void $ txt "."
  l <- labelNameP
  endPos <- getOffset
  pure $ Access (Span startPos endPos) e l
