module Language.Iota.Parser.State where

import qualified Text.Parsec as P

data ParseState = ParseState {
  indentationLevel :: P.Column
} deriving (Show)
