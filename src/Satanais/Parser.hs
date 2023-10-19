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
  [ [op "" (EBin BApp)]
  , [op "*" (EBin BMul)]
  , [op "+" (EBin BAdd), op "-" (EBin BSub)]
  , [op "==" (EBin BEql)]
  ] where op n f = InfixL (f <$ symbol n)

term :: Parser Expr
term = lexeme $ choice
  [ ELam <$> (symbol "\\" *> identifier <* symbol "->") <*> expr
  , ENum <$> L.scientific
  , EBool <$> read <$> (string "True" <|> string "False")
  , ERef <$> identifier
  , parens expr
  ]

expr :: Parser Expr
expr = makeExprParser term operations

stmt :: Parser Stmt
stmt = Def <$> (identifier <* symbol "=") <*> expr

program :: Parser Program
program = between sc eof $ stmt `sepEndBy` some eol

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = parse (program <* eof) mempty

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (expr <* eof) mempty
