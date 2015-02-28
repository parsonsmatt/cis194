module Fibonacci where
import Data.Array (array)
import Data.List (genericIndex)

-- Exercise #1:

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Exercise #2:

fibs2 :: [Integer]
fibs2 = listOFibs
    where listOFibs = [ | ]
