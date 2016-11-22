module Language.Iota.Parser.Common where

import Control.Monad (guard)

import Language.Iota.Ast.SourcePos
import Language.Iota.Parser.State
import Language.Iota.Parser.Lexer

import qualified Text.Parsec as P

mark :: P.Parsec s ParseState a -> P.Parsec s ParseState a
mark p = do
  current <- indentationLevel <$> P.getState
  pos <- P.sourceColumn <$> P.getPosition
  P.modifyState $ \st -> st { indentationLevel = pos}
  a <- p
  P.modifyState $ \st -> st { indentationLevel = current}
  return a

checkIndentation :: (P.Column -> String)
                 -> (P.Column -> P.Column -> Bool)
                 -> P.Parsec s ParseState ()
checkIndentation mkMsg rel = do
  col <- P.sourceColumn <$> P.getPosition
  current <- indentationLevel <$> P.getState
  guard (col `rel` current) P.<?> mkMsg current

indented :: P.Parsec s ParseState ()
indented = checkIndentation (("indentation past column " ++) . show) (>)

same :: P.Parsec s ParseState ()
same = checkIndentation (("indentation at column " ++) . show) (==)

runTokenParser :: FilePath -> TokenParser a -> [PositionedToken] -> Either P.ParseError a
runTokenParser filepath p = P.runParser p (ParseState 0) filepath

toSourcePos :: P.SourcePos -> SourcePos
toSourcePos pos = SourcePos (P.sourceLine pos) (P.sourceColumn pos)
