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
          where f x = (*) (x-2) 


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise =     fun2 (3 * n + 1)

-- 1 -> 0
-- 2 -> 2 + fun2 (n/2) -> 2 + 0
-- 3 -> fun2(3*3+1=10) -> 10 + fun2(5) -> fun(3*5+1=16) -> 16 + fun2(8) -> 8 + fun2(4) -> 4 + fun2(2) -> 2

fun2' :: Integer -> Integer
fun2' n = 1


