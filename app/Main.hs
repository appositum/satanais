module Main where

import Satanais
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    (arg:_) -> do
      cont <- readFile arg
      compile cont
