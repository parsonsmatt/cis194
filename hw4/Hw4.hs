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
          where f = (*) . (subtract 2) 


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise =     fun2 (3 * n + 1)

-- this is not a better implementation
fun2' :: Integer -> Integer
fun2' n = foldr f 0 list
          where f x  = (+) (if even x then x else 0)
                list = takeWhile (>1) $ iterate g n
                g y  = if even y then flip div 2 y else 1 + (*) 3 y


