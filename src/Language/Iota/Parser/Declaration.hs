module Language.Iota.Parser.Declaration where

import Language.Iota.Parser.Lexer
import Language.Iota.Ast.Declaration
import Language.Iota.Ast.SourcePos
import Language.Iota.Comments

import qualified Language.Iota.Parser.Common as C
import qualified Text.Parsec as P

withSourceSpan :: (SourceSpan -> [Comment] -> a -> a)
               -> P.Parsec [PositionedToken] u a
               -> P.Parsec [PositionedToken] u a
withSourceSpan f p = do
  start <- P.getPosition
  comments <- C.readComments
  x <- p
  end <- P.getPosition
  input <- P.getInput
  let end' = case input of
        pt:_ -> ptPrevEndPos pt
        _ -> Nothing
  let sp = SourceSpan (P.sourceName start) (C.toSourcePos start) (C.toSourcePos $ fromMaybe end end')
  return $ f sp comments x

parseValueDeclaration :: TokenParser Declaration
parseValueDeclaration = do
  name <- parseIdent
  binders <- P.many parseBinderNoParens
  value <- Left <$> (C.indented *>
                     P.many1 ((,) <$> parseGuard
                                  <*> (indented *> equals *> parseValueWithWhereClause))
                    )
           <|> Right <$> (indented *> equals *> parseValueWithWhereClause)
  return $ ValueDeclaration name Public binders value
  where
    parseValueWithWhereClause :: TokenParser Term
    parseValueWithWhereClause = do
      C.indented
      value <- parseValue
      whereClause <- P.optionMaybe $ do
        C.indented
        reserved "where"
        C.indented
        C.mark $ P.many1 (C.same *> parseLocalDeclaration)
      return $ maybe value (`Let` value) whereClause

parseDeclaration :: TokenParser Declaration
parseDeclaration = positioned (P.choice
                               [
                                 parseValueDeclaration
                               ]) P.<?> "declaration"


parseLocalDeclaration :: TokenParser Declaration
parseLocalDeclaration = positioned (P.choice
                                   [ parseValueDeclaration
                                   ] P.<?> "local declaration")



-- parseModuleFromFile :: (k -> FilePath) -> (k, Text) -> Either P.ParseError (k,Module)

parseNumericLiteral :: TokenParser (Literal a)
parseNumericLiteral = NumericLiteral <$> number

parseValue :: TokenParser Term
parseValue = withSourceSpan PositionedValue
             (P.buildExpressionParser operators . C.buildPostfixParser postfixTable $ indexersAndAccesors) P.<?> "term"

parseLet :: TokenParser Term
parseLet = do
  reserved "let"
  C.indented
  ds <- C.mark $ P.many1 (C.same *> parseLocalDeclaration)
  C.indented
  reserved "in"
  result <- parseValue
  return $ Let ds result

parseValueAtom :: TokenParser Term
parseValueAtom = withSourceSpan PositionedValue $ P.choice
               [ Literal <$> parseNumericLiteral
               , parseLet
               ]
