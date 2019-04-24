module Main where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

main :: IO ()
main = pure ()
