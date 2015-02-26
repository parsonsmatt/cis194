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
instance Expr Integer where
    lit a   = a
    add a b = a + b
    mul a b = a * b

instance Expr Bool where
    lit a | a <= 0 = False
          | a >  0 = True
    add a b = a || b
    mul a b = a && b

data MinMax = MinMax Integer
    deriving (Show, Eq, Ord)

instance Expr MinMax where
    lit a = MinMax a
    add a b = max a b
    mul a b = min a b

data Mod7 = Mod7 Integer
    deriving (Show, Eq, Ord)

instance Num Mod7 where
    fromInteger a   = Mod7 (a `mod` 7)
    Mod7 a + Mod7 b = fromInteger (a + b)
    Mod7 a * Mod7 b = fromInteger (a * b)
    Mod7 a - Mod7 b = fromInteger (a - b)
    signum (Mod7 a) = Mod7 (signum a)
    abs (Mod7 a)    = Mod7 (abs a)

instance Expr Mod7 where
    lit a = fromInteger a :: Mod7
    add a b = a + b
    mul a b = a * b
