module Hope.Pretty where

class Pretty p where
  {-# MINIMAL pretty #-}
  pretty :: p -> String

  prettyPrint :: p -> IO ()
  prettyPrint = putStrLn . pretty
