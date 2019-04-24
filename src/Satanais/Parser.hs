module Satanais.Parser where

import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Satanais.AST

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  (void $ oneOf [' ', '\t'])
  (L.skipBlockCommentNested "{-" "-}")
  (L.skipLineComment "--")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $
  (:) <$> letterChar <*> takeWhileP Nothing isAlphaNum

operations :: [[Operator Parser Expr]]
operations =
  [ [op "*" Mul]
  , [op "+" Add, op "-" Sub]
  ] where op n f = InfixL (f <$ symbol n)

term :: Parser Expr
term = lexeme $ choice
  [ Lam <$> (symbol "\\" *> identifier <* symbol "->") <*> expr
  , Num <$> (try L.float <|> fromIntegral <$> (L.decimal :: Parser Int))
  , parens expr
  ]

expr :: Parser Expr
expr = makeExprParser term operations
