module Satanais.AST where

import           Data.Scientific (Scientific)

data Expr = Num Double -- Scientific
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Lam String Expr
          deriving (Eq, Show)
