module Golf where 
import Data.List

-- Exercise 1: Hopscotch
skips :: [a] -> [[a]]
skips xs = zipWith takeEvery [1..] (map (const xs) xs)

takeEvery :: Int -> [a] -> [a]
takeEvery n xs 
    | n > length xs = []
    | otherwise     = xs !! (n-1) : takeEvery n (drop n xs)


-- Exercise 2: Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima []     = []
localMaxima [x]    = []
localMaxima (x:xs) = if x > head xs
                     then x : localMaxima xs
                     else localMaxima xs

-- Exercise 3: Histogram

histogram :: [Integer] -> String
histogram [] = "" 
histogram xs = foldr1 (\str acc -> acc ++ "\n" ++ str)
                $ removeSpaces 
                $ transpose $ composeStrings $ completeListPairs xs

-- Remove all-space strings:
removeSpaces :: [String] -> [String]
removeSpaces = filter (any (/=' '))

-- Compose total string set:
composeStrings :: [(Integer, Integer)] -> [String]
composeStrings = map buildString

-- Construct a single string:
buildString :: (Integer, Integer) -> String
buildString (x,y) = show x ++ 
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
