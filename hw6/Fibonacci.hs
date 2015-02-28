module Fibonacci where
import Data.Array (array)
import Data.List (genericTake)

-- Exercise #1:

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Exercise #2:

fibs2 :: [Integer]
fibs2 = 0 : 1 : (map (uncurry (+)) $ zip fibs2 $ tail fibs2)

-- Exercise #3:

data Stream a = Elem a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Elem x b) = [x] ++ streamToList b 

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Exercise #4:

streamRepeat :: a -> Stream a
streamRepeat x = Elem x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Elem a as) = Elem (f a) (streamMap f as)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Elem (f a) (streamFromSeed f (f a))
