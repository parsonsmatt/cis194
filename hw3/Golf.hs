-- Homework 3

module Golf where 

-- Exercise 1: Hopscotch
skips :: [a] -> [[a]]
skips [] = []
skips xs = map deIndex (map (deIndex) (flatten xs))

deIndex :: (Integral a) => [(a,b)] -> [b]
deIndex x = map snd x

flatten :: [b] -> [[(Int, (Int, b))]]
flatten xs = map dropIndex $ buildList xs

-- Drops elements in the list if they're divisible by the number.
dropIndex :: (Int,[a]) -> [(Int,a)]
dropIndex (n,xs) = filter (\(x,_) -> x `mod` n == 0) (index xs)

-- Takes a list, indexes it, expands it into a list of lists, and
-- indexes the top level list.
buildList :: [a] -> [(Int, [(Int, a)])]
buildList = index . expandList . index

-- Takes a list and converts it into a list of lists, each of which
-- is the original list.
expandList :: [a] -> [[a]]
expandList xs = map (\x -> xs) xs

-- Takes a list and indexes the items.
index :: [a] -> [(Int,a)]
index = zip [1..]

-- Exercise 2: Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []


-- Exercise 3: Histogram

histogram :: [Integer] -> String
histogram [] = ""
