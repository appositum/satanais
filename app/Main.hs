module Main where

import Control.Monad
import System.IO
import Satanais

main :: IO ()
main = repl

repl :: IO ()
repl = do
  input <- putStr "Satanais> " *> hFlush stdout *> getLine
  unless (input == ":quit" || input == ":q") $ do
    if null input
      then repl
      else putStrLn input *> repl
