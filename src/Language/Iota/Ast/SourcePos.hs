module Language.Iota.Ast.SourcePos where

data SourcePos = SourcePos {
    sourcePosLine :: Int
  , sourcePosColumn :: Int
} deriving (Show, Eq, Ord)

data SourceSpan = SourceSpan {
    spanName :: String
  , spanStart :: SourcePos
  , spanEnd :: SourcePos
  } deriving (Show, Eq, Ord)
