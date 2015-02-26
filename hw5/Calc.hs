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

-- Exercise #3:
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit a   = Lit a
    add a b = Add a b
    mul a b = Mul a b

-- Exercise #4:

