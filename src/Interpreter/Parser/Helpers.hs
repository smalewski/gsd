{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Interpreter.Parser.Helpers where

import Interpreter.Span

import Control.Monad.Reader
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char (digitChar, letterChar, lowerChar, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L ( lexeme, skipBlockComment
                                                 , skipLineComment, space
                                                 , indentLevel, indentGuard, nonIndented )
import GHC.Unicode (isLower, isUpper, isDigit)
import Interpreter.Type (Type)
import Interpreter.Syntax.Common (DataName(DataName), LabelName(LabelName), CtorName(CtorName), Name(Name), Openess (Closed))
import qualified Data.Set as Set
import Data.List (group, sort)
import Data.Maybe (listToMaybe)
import Prelude hiding (span)

type IndentLevel = Pos

data ParseMode = Normal | SameLine
  deriving (Eq, Show)

data ParseState = ParseState { psIndentLevel :: IndentLevel
                             , psParseMode :: ParseMode
                             }
  deriving (Eq, Show)

type Parser = ParsecT Error Text (Reader ParseState)
type ParseError = ParseErrorBundle Text Error
type ParseResult = Either ParseError

data Error = EArity CtorName
           | ELabels CtorName
           | EPosUnclass CtorName
           | EFunType Type
           | ETopLevel Span
           | EFunVars Name
           | EKeyword Span Text
           deriving (Eq, Ord)

instance HasSpan Error where
  span (EArity c) = span c
  span (ELabels c) = span c
  span (EPosUnclass c) = span c
  span (EFunType t) = span t
  span (ETopLevel s) = s
  span (EFunVars x) = span x
  span (EKeyword s _) = s

customFailure' :: Error -> Parser a
customFailure' err = do
  setOffset . beginPos $ span err
  customFailure err

instance ShowErrorComponent Error where
  showErrorComponent (EArity c)
    = "Ambiguos arity in constructor."
  showErrorComponent (ELabels c)
    = "Inconsistent labels in constructor."
  showErrorComponent (EPosUnclass c)
    = "Unclassified data cannot be instantiated positionally."
  showErrorComponent (EFunType t)
    = "Type in declaration is not consistent with arrow."
  showErrorComponent (ETopLevel _)
    = "Top level declarations or expressions cannot be indented."
  showErrorComponent (EFunVars x)
    = "Conflicting definition for " <> show x <> "."
  showErrorComponent (EKeyword _ k)
    = "Expecting keyword " <> unpack k <> "."

-- | Higher order parser that parses all trailing whitespace after the given parser.
lexeme :: Parser a -> Parser a
lexeme p = asks psParseMode >>= \case
  Normal -> L.lexeme whitespace p
  SameLine -> L.lexeme whitespaceSameLine p

-- | Same as lexeme, but takes last indent level into account (for example in a linefold)
lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme whitespace'

-- | Parser that consumes whitespace in general.
spaces :: Parser ()
spaces = L.space spaceParser commentParser blockCommentParser where
  spaceParser = skipSome (char ' ')
  commentParser = L.skipLineComment "--"
  blockCommentParser = L.skipBlockComment "{-" "-}"

whitespace :: Parser ()
whitespace = L.space spaceParser commentParser blockCommentParser where
  spaceParser = skipSome wsChar
  commentParser = L.skipLineComment "--"
  blockCommentParser = L.skipBlockComment "{-" "-}"

-- | Same as whitespace, but takes last indent level into account (e.g. in a line fold)
whitespace' :: Parser ()
whitespace' = try $ do
  lastIndentLvl <- asks psIndentLevel
  void $ L.indentGuard whitespace GT lastIndentLvl

whitespaceSameLine :: Parser ()
whitespaceSameLine = L.space (skipSome ws) empty empty where
  ws = char ' ' <?> "whitespace"

-- | Helper for parsing a chunk of text, with everything on same line.
sameLine :: Parser a -> Parser a
sameLine p = do
  parseState <- ask
  local (const $ parseState { psParseMode = SameLine }) p

-- | Helper for parsing a line fold (parser spanning multiple lines, with lines after
--   beginning line requiring greater indentation). Tries to parse whitespace after the linefold.
withLineFold :: Parser a -> Parser a
withLineFold p = lexeme $ do
  whitespace
  currentIndent <- L.indentLevel
  parseState <- ask
  local (const $ parseState { psIndentLevel = currentIndent }) p

withIndent :: Pos -> Parser a -> Parser a
withIndent indent p = L.indentGuard whitespace EQ indent *> p

-- | Helper for only parsing if a word occurs with indentation > 1.
--   Assumes space in front of the token to be parsed has already been parsed.
--   This works best for declarations that can only appear at top level in
--   combination with lexeme (instead of lexeme' in a linefold).
indented :: Parser a -> Parser a
indented p = L.indentGuard (pure ()) GT pos1 *> p

topLevel :: Parser a -> Parser a
topLevel p = try $ do
  whitespace
  isEnd <- lookAhead atEnd
  o <- getOffset
  when isEnd empty
  actual <- L.indentLevel
  if actual == pos1
    then p
    else customFailure' $ ETopLevel (Span o o)

wsChar :: Parser ()
wsChar = void (char ' ' <|> char '\n') <?> "whitespace"

betweenParens :: Parser a -> Parser a
betweenParens = between (lexeme' $ char '(') (char ')') . lexeme'

betweenOptionalParens :: Parser a -> Parser a
betweenOptionalParens p = betweenParens p <|> p

-- | Helper for optionally parsing a value. If the parsing fails,
--   it will use the provided default value instead.
withDefault :: a -> Parser a -> Parser a
withDefault def p = p <|> pure def

char :: Char -> Parser Char
char = single

-- | Helper function for creating a parser that consumes a keyword.
--   Expects trailing whitespace after the actual keyword.
keyword :: Text -> Parser ()
keyword s = do
  o <- getOffset
  lexeme' (chunk s *> lookAhead wsChar) <|> customFailure (EKeyword (Span o o) s)

txt :: Text -> Parser ()
txt s = () <$ lexeme (chunk s)
{-
-- | Helper function for creating a parser that consumes a keyword
--   with optional trailing whitespace. The return value indicates
--   if it consumed whitespace or not.
keyword' :: Text -> Parser KeywordResult
keyword' s = try (keyword s $> TrailingWS)
          <|> chunk s $> NoTrailingWS
-}

identifier :: Parser Text
identifier = do
  firstChar <- lowerChar
  rest <- takeWhileP (Just "rest of identifier") isIdentifierChar
  let parsed = T.cons firstChar rest
  when (parsed `V.elem` reserved) $
    fail . T.unpack $ "Reserved keyword: " <> parsed
  pure parsed

reserved :: V.Vector Text
reserved =
  [ "data", "let", "in"
  , "if", "then", "else"
  , "match", "with"
  , "open", "closed"
  ]

reservedKeywords :: Parser ()
reservedKeywords = choice $ keyword <$> reserved

capitalIdentifier :: Parser Text
capitalIdentifier = do
  firstChar <- upperChar
  rest <- takeWhileP (Just "rest of identifier") isIdentifierChar
  let parsed = T.cons firstChar rest
  when (parsed `V.elem` capReserved) $
    fail . T.unpack $ "Reserved keyword: " <> parsed
  pure parsed

capReserved :: V.Vector Text
capReserved = [ "Int", "String" ]

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isLower c || isUpper c || isDigit c || c == '\''

opIdentifier :: Parser Text
opIdentifier = do
  first <- satisfy isOperatorChar <?> "operator"
  rest <- takeWhileP (Just "rest of operator") isOperatorChar
  let parsed = T.cons first rest
  when (parsed `V.elem` reservedOps) (fail . T.unpack $ "Reserved operator: '" <> parsed <> "'")
  notFollowedBy $ satisfy isDigit
  pure parsed
  where
    reservedOps = [ "+", "-", "*", "/", ":", "=", "\\", "|", "->", "=>" ]

isOperatorChar :: Char -> Bool
isOperatorChar c = c `VU.elem` opChars

opChars :: VU.Vector Char
opChars = ['!', '#', '$', '%', '&', '.', '+', '*', '/', '<', '>'
          , '=', '?', '@', '\\', '^', '|', '-', '~', ':']

singleQuote :: Parser Char
singleQuote = char '\''

-- | Helper function for getting the begin and end offset when parsing something.
--   Important: use this function before lexeme/lexeme' or the trailing
--   whitespace will also be counted!
withSpan :: Parser a -> Parser (Span, a)
withSpan p = do
  begin <- getOffset
  result <- p
  end <- getOffset
  pure (Span begin end, result)

nameP :: Parser Name
nameP = uncurry Name <$> withSpan (lexeme identifier) <?> "identifier"

dataNameP :: Parser DataName
dataNameP = uncurry DataName <$> withSpan (lexeme capitalIdentifier) <*> pure Closed <?> "datatype"

ctorNameP :: Parser CtorName
ctorNameP = uncurry CtorName <$> withSpan (lexeme capitalIdentifier) <?> "constructor's name"

labelNameP :: Parser LabelName
labelNameP = uncurry LabelName <$> withSpan (lexeme identifier) <?> "label"

getDuplicate :: Ord a => [a] -> Maybe a
getDuplicate = listToMaybe . concat . filter (\x -> length x > 1) . group . sort
