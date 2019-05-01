module Satanais.AST where

import Data.Scientific (Scientific)

type Number = Scientific

data BinOp = BAdd
           | BMul
           | BSub
           | BApp
           | BEql
           deriving (Eq, Show)

data Expr = ENum Number
          | EBool Bool
          | ERef String
          | EBin BinOp Expr Expr
          | ELam String Expr
          deriving (Eq, Show)

data Stmt = Def String Expr
  deriving (Eq, Show)

type Program = [Stmt]
