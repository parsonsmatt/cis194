module Hw4 where
import Data.List hiding (insert)
import Data.Functor

-- Exercise 1:

fun1 :: [Integer] -> Integer
fun1 []       = 1
fun1 (x:xs) 
  | even x    = (x - 2) * fun1 xs
  | otherwise =           fun1 xs

fun1' :: [Integer] -> Integer
fun1' =  product . filter even . map (subtract 2)

fun1'' :: [Integer] -> Integer
fun1'' = foldr f 1
         where f x acc | even x    = acc * (x-2) 
                       | otherwise = acc

fun1''' :: [Integer] -> Integer
fun1''' = foldr f 1 . filter even
          where f = (*) . subtract 2 


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise =     fun2 (3 * n + 1)

-- this is not a better implementation
fun2' :: Integer -> Integer
fun2' n = foldr f 0 list
          where f x | even x    = (+) x 
                    | otherwise = (+) 0
                g y | even y    = y `div` 2 
                    | otherwise = 1 + (*) 3 y
                list = takeWhile (>1) $ iterate g n

-- Exercise 2:

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Eq, Show)

foldTree :: (Show a) => [a] -> Tree a
foldTree = foldr insert Leaf

-- Skipping this...
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h Leaf n Leaf) = Node (h+1) (insert x Leaf) n Leaf
insert x (Node h r n Leaf ) = Node h r n (insert x Leaf)

full :: Tree a -> Bool
full Leaf = False
full (Node _ Leaf _ Leaf) = True
full (Node _ Leaf _ Node{}) = False
full (Node _ Node{} _ Leaf) = False
full (Node _ r _ l) = full r && full l


height :: (Integral b) => Tree a -> b
height Leaf = -1
height (Node _ l _ r) = 1 + max (height l) (height r)

nodes :: (Integral b) => Tree a -> b
nodes Leaf = 0
nodes (Node _ l _ r) = 1 + (nodes l + nodes r)

-- Exercise 3:

xor :: [Bool] -> Bool
xor = foldl1' (/=)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- myFoldl :: (a -> b -> a) -> a [b] -> a
-- myFoldl f base xs = foldr

-- Exercise 4:

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) <$> (*2)) $ list
                where list = upToN \\ exclude
                      exclude = foldl f [] $ cartProd upToN upToN
                      f acc (i,j) = if i <= j && g i j <= n 
                                       then acc ++ [g i j]
                                       else acc
                      g i j = i + j + (2 * i * j)
                      upToN = [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

{---- 
- Start with a list of integers from 1 to n. From this list, remove all
- numbers of the form i + j + 2ij where:
- * i, j in N, 1 <= i <= j
- * i + j + 2ij <= n 
- The remaining numbers are doubled and incremented by one, giving a list
- of the odd prime numbers below 2n + 2.
-}

