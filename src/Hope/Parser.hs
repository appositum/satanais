module Hope.Parser where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Hope.AST

type Parser = Parsec Void String

boolean :: Parser AST
boolean = Boolean <$> read <$> (string "True" <|> string "False")

number :: Parser AST
number = Number <$> signed mempty decimal

list :: Parser AST
list = List <$> between (char '[') (char ']') (expression `sepBy` char ',')

expression :: Parser AST
expression =  boolean
          <|> number
          <|> list
