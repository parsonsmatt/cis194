module Hw4 where

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
                g y | even y    = flip div 2 y 
                    | otherwise = 1 + (*) 3 y
                list = takeWhile (>1) $ iterate g n

-- Exercise 2:

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Eq, Show)

foldTree :: [a] -> Tree a
foldTree xs = foldr insert Leaf xs

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h l n r)
    | hl < hr   = Node h (insert x l) n r
    | hl > hr   = Node h l n ixr
    | otherwise = Node (h+1) l n ixr
  where hl  = height l
        hr  = height r
        ixr = insert x r
        h   = height ixr

complete :: Tree a -> Bool
complete Leaf = True
complete (Node _ Leaf _ (Node _ _ _ _)) = False
complete (Node _ (Node _ _ _ _) _ Leaf) = False
complete (Node _ r _ l) = complete r && complete l


height :: (Integral b) => Tree a -> b
height Leaf = -1
height (Node _ l _ r) = 1 + max (height l) (height r)

nodes :: (Integral b) => Tree a -> b
nodes Leaf = 0
nodes (Node _ l _ r) = 1 + (nodes l + nodes r)
