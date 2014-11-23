-- Homework 3

module Golf where 

-- Exercise 1: Hopscotch

-- The arrays in the produced array consist of the original array, only 
-- consisting of elements whose index is divisible by the number of the 
-- array.

skips :: [a] -> [[a]]
skips [] = []
skips xs = map deIndex ((map deIndex) (flatten xs))

deIndex :: (Integral a) => [(a,b)] -> [b]
deIndex x = map snd x

flatten :: [b] -> [[(Int, (Int, b))]]
flatten xs = map dropIndex $ buildList xs

--buildList :: [(Int, [(Int, a)])]
buildList = withIndex . expandList . withIndex

--expandList :: [a] -> [[a]]
expandList xs = map (\x -> xs) xs

--withIndex :: [a] -> [(Int,a)]
withIndex = zip [1..]

--dropIndex :: (Int,[a]) -> [(Int,a)]
dropIndex (n,xs) = filter (\(x,_) -> x `mod` n == 0) (withIndex xs)

-- Exercise 2: Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []


-- Exercise 3: Histogram

histogram :: [Integer] -> String
histogram [] = ""
