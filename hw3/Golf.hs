module Golf where 
import Data.List

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

-- Exercise 1, take 2:
skips' :: [a] -> [[a]]
skips xs = 


-- Exercise 2: Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima []     = []
localMaxima [x]    = []
localMaxima (x:xs) = if (x > head xs) 
                     then x : (localMaxima xs)
                     else localMaxima xs

-- Exercise 3: Histogram

histogram :: [Integer] -> String
histogram [] = "" 
histogram xs = foldr1 (\str acc -> acc ++ "\n" ++ str)
                $ removeSpaces 
                $ transpose $ composeStrings $ completeListPairs xs

-- Remove all-space strings:
removeSpaces :: [String] -> [String]
removeSpaces = filter (any (\char -> char /= ' '))

-- Compose total string set:
composeStrings :: [(Integer, Integer)] -> [String]
composeStrings xs = map buildString xs

-- Construct a single string:
buildString :: (Integer, Integer) -> String
buildString (x,y) = (show x) ++ 
                    "=" ++ 
                    genericReplicate y '*' ++
                    genericReplicate (9-y) ' '

-- Construct the list of numbers and occurences
completeListPairs xs = sort $ union (missingNum xs) (numsAndOccurences xs)

-- A list of missing numbers, paired with 0.
missingNum xs = zip ([0..9] \\ xs) (repeat 0)

-- Pairs of numbers in list and their occurences.
numsAndOccurences :: [Integer] -> [(Integer, Integer)]
numsAndOccurences xs = zip (numbersInList xs) (occurences xs)

-- Gives the numbers in the sorted list.
numbersInList :: [Integer] -> [Integer]
numbersInList xs = nub $ sort xs

-- Gives the number of occurences for each number in the sorted list.
occurences :: [Integer] -> [Integer]
occurences xs = map (fromIntegral . length) $ group $ sort xs
