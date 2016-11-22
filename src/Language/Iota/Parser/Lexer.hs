module Language.Iota.Parser.Lexer where

import Language.Iota.Comments
import Language.Iota.Parser.State

import Prelude hiding (lex)

import Control.Applicative
import Control.Monad (void, guard)
import Data.Char (isSpace,isSymbol,isAscii)
import Data.Functor.Identity
import Data.Text (Text)

import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT

data Token
     = LParen
     | RParen
     | LBrace
     | RBrace
     | RBracket
     | LBracket
     | LArrow
     | LFatArrow
     | RArrow
     | RFatArrow
     | Colon
     | DoubleColon
     | Equals
     | Pipe
     | Tick
     | Dot
     | Comma
     | Semi
     | At
     | Underscore
     | Indent Int
     | LName String
     | UName String
     | Qualifier String
     | Symbol String
     | CharLiteral Char
     | StringLiteral String
     | Number (Either Integer Double)
     deriving (Show,Eq,Ord)

prettyPrintToken LParen = "("
prettyPrintToken RParen = ")"
prettyPrintToken LBrace = "{"
prettyPrintToken RBrace = "}"
prettyPrintToken RBracket = "]"
prettyPrintToken LBracket = "["
prettyPrintToken LArrow = "->"
prettyPrintToken LFatArrow = "=>"
prettyPrintToken RArrow = "<-"
prettyPrintToken RFatArrow = "=>"
prettyPrintToken Colon = ":"
prettyPrintToken DoubleColon = "::"
prettyPrintToken Equals = "="
prettyPrintToken Pipe = "|"
prettyPrintToken Tick = "`"
prettyPrintToken Dot = "."
prettyPrintToken Comma             = ","
prettyPrintToken Semi              = ";"
prettyPrintToken At                = "@"
prettyPrintToken Underscore        = "_"
prettyPrintToken (Indent n)        = "indentation at level " ++ show n
prettyPrintToken (LName s)         = show s
prettyPrintToken (UName s)         = show s
prettyPrintToken (Qualifier _)     = "qualifier"
prettyPrintToken (Symbol s)        = s
prettyPrintToken (CharLiteral c)   = show c
prettyPrintToken (StringLiteral s) = show s
prettyPrintToken (Number n)        = either show show n


type Lexer u a = P.Parsec Text u a

data PositionedToken = PositionedToken {
    ptSourcePos :: P.SourcePos
  , ptEndPos :: P.SourcePos
  , ptPrevEndPos :: Maybe P.SourcePos
  , ptToken :: Token
  , ptComments :: [Comment]
} deriving (Eq)


instance Show PositionedToken where
  show = prettyPrintToken . ptToken

lex :: FilePath -> String -> Either P.ParseError [PositionedToken]
lex fp = lex' fp . T.pack

lex' :: FilePath -> Text -> Either P.ParseError [PositionedToken]
lex' f s = updatePositions <$> P.parse parseTokens f s

updatePositions :: [PositionedToken] -> [PositionedToken]
updatePositions [] = []
updatePositions (x:xs) = x : zipWith update (x:xs) xs
  where update PositionedToken { ptEndPos = pos } pt = pt { ptPrevEndPos = Just pos }

parseTokens :: Lexer u [PositionedToken]
parseTokens = whitespace *> P.many parsePositionedToken <* P.skipMany parseComment <* P.eof

whitespace :: Lexer u ()
whitespace = P.skipMany (P.satisfy isSpace)

parseComment :: Lexer u Comment
parseComment = (LineComment <$> lineComment) <* whitespace
  where lineComment :: Lexer u String
        lineComment = P.try $ P.string "--" *> P.manyTill P.anyChar (P.try (void (P.char '\n') <|> P.eof))

parsePositionedToken :: Lexer u PositionedToken
parsePositionedToken = P.try $ do
  comments <- P.many parseComment
  pos <- P.getPosition
  tok <- parseToken
  pos' <- P.getPosition
  whitespace
  return $ PositionedToken pos pos' Nothing tok comments

parseToken :: Lexer u Token
parseToken = P.choice
  [ P.try $ P.string "<-" *> P.notFollowedBy symbolChar *> pure LArrow
  , P.try $ P.string "<=" *> P.notFollowedBy symbolChar *> pure LFatArrow
  , P.try $ P.string "->" *> P.notFollowedBy symbolChar *> pure RArrow
  , P.try $ P.string "=>" *> P.notFollowedBy symbolChar *> pure RFatArrow
  , P.try $ P.string "::" *> P.notFollowedBy symbolChar *> pure DoubleColon
  , P.try $ P.char '(' *> pure LParen
  , P.try $ P.char ')' *> pure RParen
  , P.try $ P.char ']' *> pure RBracket
  , P.try $ P.char '[' *> pure LBracket
  , P.try $ P.char '{' *> pure LBrace
  , P.try $ P.char '}' *> pure RBrace
  , P.try $ P.char '`'    *> pure Tick
  , P.try $ P.char ','    *> pure Comma
  , P.try $ P.char '='    *> P.notFollowedBy symbolChar *> pure Equals
  , P.try $ P.char ':'    *> P.notFollowedBy symbolChar *> pure Colon
  , P.try $ P.char '|'    *> P.notFollowedBy symbolChar *> pure Pipe
  , P.try $ P.char '.'    *> P.notFollowedBy symbolChar *> pure Dot
  , P.try $ P.char ';'    *> P.notFollowedBy symbolChar *> pure Semi
  , P.try $ P.char '@'    *> P.notFollowedBy symbolChar *> pure At
  , P.try $ P.char '_'    *> P.notFollowedBy identLetter *> pure Underscore
  , LName         <$> parseLName
  , parseUName >>= \uName ->
      (guard (validModuleName uName) >> Qualifier uName <$ P.char '.')
        <|> pure (UName uName)
  , Symbol        <$> parseSymbol
  , CharLiteral   <$> parseCharLiteral
  , StringLiteral <$> parseStringLiteral
  , Number        <$> parseNumber
  ]
  where
    parseLName :: Lexer u String
    parseLName = (:) <$> identStart <*> P.many identLetter

    parseUName :: Lexer u String
    parseUName = (:) <$> P.upper <*> P.many identLetter

    parseSymbol :: Lexer u String
    parseSymbol = P.many1 symbolChar

    identStart :: Lexer u Char
    identStart = P.lower <|> P.oneOf "_"

    identLetter :: Lexer u Char
    identLetter = P.alphaNum <|> P.oneOf "_'"

    symbolChar :: Lexer u Char
    symbolChar = P.satisfy isSymbolChar

    parseCharLiteral :: Lexer u Char
    parseCharLiteral = PT.charLiteral tokenParser

    parseStringLiteral :: Lexer u String
    parseStringLiteral = blockString <|> PT.stringLiteral tokenParser
      where
        delimiter   = P.try (P.string "\"\"\"")
        blockString = delimiter >> P.manyTill P.anyChar delimiter

    parseNumber :: Lexer u (Either Integer Double)
    parseNumber = (consumeLeadingZero >> P.parserZero) <|>
                    (Right <$> P.try (PT.float tokenParser) <|>
                     Left <$> P.try (PT.natural tokenParser))
                    P.<?> "number"
                    where
                      consumeLeadingZero = P.lookAhead (P.char '0' >>
                                                        (P.notFollowedBy P.digit P.<?> "no leading zero in number literal"))


langDef :: PT.GenLanguageDef Text u Identity
langDef = PT.LanguageDef
  { PT.reservedNames   = []
  , PT.reservedOpNames = []
  , PT.commentStart    = ""
  , PT.commentEnd      = ""
  , PT.commentLine     = ""
  , PT.nestedComments  = True
  , PT.identStart      = fail "Identifiers not supported"
  , PT.identLetter     = fail "Identifiers not supported"
  , PT.opStart         = fail "Operators not supported"
  , PT.opLetter        = fail "Operators not supported"
  , PT.caseSensitive   = True
  }

tokenParser :: PT.GenTokenParser Text u Identity
tokenParser = PT.makeTokenParser langDef

type TokenParser a = P.Parsec [PositionedToken] ParseState a

anyToken :: TokenParser PositionedToken
anyToken = P.token (prettyPrintToken . ptToken) ptSourcePos Just

token :: (Token -> Maybe a) -> TokenParser a
token f = P.token (prettyPrintToken . ptToken) ptSourcePos (f . ptToken)

match :: Token -> TokenParser ()
match tok = token (\tok' -> if tok == tok' then Just () else Nothing) P.<?> prettyPrintToken tok

lparen :: TokenParser ()
lparen = match LParen

rparen :: TokenParser ()
rparen = match RParen

parens :: TokenParser a -> TokenParser a
parens = P.between lparen rparen

lbrace :: TokenParser ()
lbrace = match LBrace

rbrace :: TokenParser ()
rbrace = match RBrace

braces :: TokenParser a -> TokenParser a
braces = P.between lbrace rbrace

lbracket :: TokenParser ()
lbracket = match LBracket

rbracket :: TokenParser ()
rbracket = match RBracket

brackets :: TokenParser a -> TokenParser a
brackets = P.between lbracket rbracket

indent :: TokenParser Int
indent = token go P.<?> "indentation"
  where
  go (Indent n) = Just n
  go _ = Nothing

indentAt :: P.Column -> TokenParser ()
indentAt n = token go P.<?> "indentation at level " ++ show n
  where
  go (Indent n') | n == n' = Just ()
  go _ = Nothing

larrow :: TokenParser ()
larrow = match LArrow

rarrow :: TokenParser ()
rarrow = match RArrow

lfatArrow :: TokenParser ()
lfatArrow = match LFatArrow

rfatArrow :: TokenParser ()
rfatArrow = match RFatArrow

colon :: TokenParser ()
colon = match Colon

doubleColon :: TokenParser ()
doubleColon = match DoubleColon

equals :: TokenParser ()
equals = match Equals

pipe :: TokenParser ()
pipe = match Pipe

tick :: TokenParser ()
tick = match Tick

dot :: TokenParser ()
dot = match Dot

comma :: TokenParser ()
comma = match Comma

semi :: TokenParser ()
semi = match Semi

at :: TokenParser ()
at = match At

underscore :: TokenParser ()
underscore = match Underscore

semiSep :: TokenParser a -> TokenParser [a]
semiSep = flip P.sepBy semi

semiSep1 :: TokenParser a -> TokenParser [a]
semiSep1 = flip P.sepBy1 semi

commaSep :: TokenParser a -> TokenParser [a]
commaSep = flip P.sepBy comma

commaSep1 :: TokenParser a -> TokenParser [a]
commaSep1 = flip P.sepBy1 comma

lname :: TokenParser String
lname = token go P.<?> "identifier"
  where
  go (LName s) = Just s
  go _ = Nothing

lname' :: String -> TokenParser ()
lname' s = token go P.<?> show s
  where
  go (LName s') | s == s' = Just ()
  go _ = Nothing

qualifier :: TokenParser String
qualifier = token go P.<?> "qualifier"
  where
  go (Qualifier s) = Just s
  go _ = Nothing

reserved :: String -> TokenParser ()
reserved s = token go P.<?> show s
  where
  go (LName s') | s == s' = Just ()
  go (Symbol s') | s == s' = Just ()
  go _ = Nothing

uname :: TokenParser String
uname = token go P.<?> "proper name"
  where
  go (UName s) | validUName s = Just s
  go _ = Nothing

uname' :: String -> TokenParser ()
uname' s = token go P.<?> "proper name"
  where
  go (UName s') | s == s' = Just ()
  go _ = Nothing

tyname :: TokenParser String
tyname = token go P.<?> "type name"
  where
  go (UName s) = Just s
  go _ = Nothing

dconsname :: TokenParser String
dconsname = token go P.<?> "data constructor name"
  where
  go (UName s) = Just s
  go _ = Nothing

mname :: TokenParser String
mname = token go P.<?> "module name"
  where
  go (UName s) | validModuleName s = Just s
  go _ = Nothing

symbol :: TokenParser String
symbol = token go P.<?> "symbol"
  where
  go (Symbol s) = Just s
  go Colon      = Just ":"
  go LFatArrow  = Just "<="
  go At         = Just "@"
  go _ = Nothing

symbol' :: String -> TokenParser ()
symbol' s = token go P.<?> show s
  where
  go (Symbol s') | s == s'   = Just ()
  go Colon       | s == ":"  = Just ()
  go LFatArrow   | s == "<=" = Just ()
  go _ = Nothing

charLiteral :: TokenParser Char
charLiteral = token go P.<?> "char literal"
  where
  go (CharLiteral c) = Just c
  go _ = Nothing

stringLiteral :: TokenParser String
stringLiteral = token go P.<?> "string literal"
  where
  go (StringLiteral s) = Just s
  go _ = Nothing

number :: TokenParser (Either Integer Double)
number = token go P.<?> "number"
  where
  go (Number n) = Just n
  go _ = Nothing

natural :: TokenParser Integer
natural = token go P.<?> "natural"
  where
  go (Number (Left n)) = Just n
  go _ = Nothing

identifier :: TokenParser String
identifier = token go P.<?> "identifier"
  where
  go (LName s) | s `notElem` reservedIoNames = Just s
  go _ = Nothing

validModuleName :: String -> Bool
validModuleName s = '_' `notElem` s

validUName :: String -> Bool
validUName s = '\'' `notElem` s

reservedIoNames :: [String]
reservedIoNames = [ "data"
                  , "newtype"
                  , "type"
                  , "import"
                  , "infixl"
                  , "infixr"
                  , "infix"
                  , "class"
                  , "instance"
                  , "derive"
                  , "module"
                  , "match"
                  , "with"
                  , "if"
                  , "then"
                  , "else"
                  , "do"
                  , "let"
                  , "rec"
                  , "in"
                  , "where"
                  ]

reservedTypeNames :: [String]
reservedTypeNames = [ "forall", "where" ]


isSymbolChar :: Char -> Bool
isSymbolChar c = (c `elem` ":!#$%&*+./<=>?@\\^|-~") || (not (isAscii c) && isSymbol c)

