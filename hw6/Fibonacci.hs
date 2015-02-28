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
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise #3:

data Stream a = Elem a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Elem x b) = x : streamToList b 

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Exercise #4:

instance Functor Stream where
    fmap f (Elem a as) = Elem (f a) (fmap f as)

streamRepeat :: a -> Stream a
streamRepeat x = Elem x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = fmap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Elem a (streamFromSeed f (f a))

-- Exercise #5:

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- nth element in the stream (assuming first element n=1) is the largest power
-- of 2 which evenly divides n: n=8 => 3 (2^3), n=6 => 1 (2^1)
-- #1: 0 will be every other element. Removing 0s gives pattern:
--      2, 4, 6, 8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40
--      1, 2, 1, 3, 1, 2, 1, 4, 1, 2, 1, 3, 1, 2, 1, 5, 1, 2, 1, 3
-- Every other is 1, so we can further reduce:
--      #:  4, 8,12,16,20,24,28,32,36,40,44,48,52,
--      2s: 2, 3, 2, 4, 2, 3, 2, 5, 2, 3, 2, 4, 2,
-- Every other is 2, so we can reduce:
--      #:  8, 16, 24, 32, 40, 48, 56, 64, 72, 80
--      2s: 3,  4,  3,  5,  3,  4,  3,  6,  3,  4
-- Every other is 3, so we can reduce:
--      #:  16, 32, 48, 64, 80
--      2s:  4,  5,  4,  6,  4
-- Ah hah! So the ruler stream is just a series of streams that are interleaved
-- with each other. 
--      ruler = interleave (streamRepeat 0) 
--              (interleave (streamRepeat 1) 
--              (interleave (streamRepeat 2)
--              (interleave (streamRepeat 3)
--              (interleave (streamRepeat 4)
--              ...))))...

ruler :: Stream Integer
ruler = go 0
    where go n = Elem n (interleaveStreams (go (n+1)) (streamRepeat n))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Elem a as) (Elem b bs) = Elem a (Elem b (interleaveStreams as bs))

-- Exercise 6:

x :: Stream Integer
x = Elem 0 (Elem 1 (streamRepeat 0))
