{-# language LambdaCase #-}

module Satanais.Eval where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as M

import           Satanais.AST
import           Satanais.Parser

data Value = VNum Number
           | VLam String Expr
           deriving (Eq, Show)

type Env = Map String Value
type Runtime = StateT Env (Except String)

applyNum :: (Number -> a) -> Value -> Runtime a
applyNum f (VNum a) = pure (f a)
applyNum _ _ = throwError "Expected a number"

applyNum2 :: (Number -> Number -> Number) -> Value -> Value -> Runtime Number
applyNum2 f x y = applyNum f x >>= \f' -> applyNum f' y

numOp :: (Number -> Number -> Number) -> Expr -> Expr -> Runtime Value
numOp f e1 e2 = fmap VNum . join $ applyNum2 f <$> eval e1 <*> eval e2

eval :: Expr -> Runtime Value
eval (ENum n) = pure (VNum n)
eval (ERef x) =
  M.lookup x <$> get >>= \case
    Just x' -> pure x'
    Nothing -> throwError $ "Couldn't find variable " ++ x
eval (EAdd e1 e2) = numOp (+) e1 e2
eval (EMul e1 e2) = numOp (*) e1 e2
eval (ESub e1 e2) = numOp (-) e1 e2
eval (ELam arg body) = pure (VLam arg body)
eval (EApp f a) = do
  f' <- eval f
  case f' of
    VLam val e -> do
      a' <- eval a
      withStateT (M.insert val a') (eval e)
    _ -> throwError "Function is not a lambda"

define :: Stmt -> Runtime ()
define (Def name value) =
  eval value >>= modify . M.insert name

run :: Program -> Either String Value
run stmts =
  fmap fst . runExcept $
    (flip runStateT) M.empty $
      traverse define stmts *> eval (ERef "main")
