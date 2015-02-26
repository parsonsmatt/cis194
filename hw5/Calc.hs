module Calc where
import ExprT
import Parser
import Data.Functor

-- Exercise #1:
eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- Exercise #2:
evalStr :: String -> Maybe Integer
evalStr str = eval <$> parseExp Lit Add Mul str
