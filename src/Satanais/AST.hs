module Satanais.AST where

import Data.Scientific (Scientific)

type Number = Scientific

data Expr = ENum Number
          | ERef String
          | EAdd Expr Expr
          | EMul Expr Expr
          | ESub Expr Expr
          | ELam String Expr
          | EApp Expr Expr
          deriving (Eq, Show)

data Stmt = Def String Expr
  deriving (Eq, Show)
