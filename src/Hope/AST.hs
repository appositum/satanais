module Hope.AST where

import Data.List (intercalate)
import Hope.Pretty

data AST = Number Int
         | Str String
         | Atom String
         | Boolean Bool
         | List [AST]
         deriving (Show, Eq)

instance Pretty AST where
  pretty (Number n) = show n
  pretty (Str s) = show s
  pretty (Atom a) = a
  pretty (Boolean b) = show b
  pretty (List xs) = "[" ++ intercalate ", " (pretty <$> xs) ++ "]"
