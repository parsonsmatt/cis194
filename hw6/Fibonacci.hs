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
fibs2 = 0 : 1 : zipWith (+) (fibs2) (tail fibs2)

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

evens :: Stream Integer
evens = streamFromSeed (+2) 0

-- nth element in the stream (assuming first element n=1) is the largest power
-- of 2 which evenly divides n: n=8 => 3 (2^3), n=6 => 1 (2^1)
ruler :: Stream Integer
ruler = nats

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Elem a as) (Elem b bs) = Elem a (Elem b (interleaveStreams as bs))

zipStreams :: Stream a -> Stream b -> Stream (a,b)
zipStreams (Elem a as) (Elem b bs) = Elem (a,b) (zipStreams as bs)
