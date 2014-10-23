-- Exercise 1:

toDigits :: Integer -> [Integer]
toDigits n
	| n <= 0    = []
	| n <= 9    = [n]
	| otherwise = concat [toDigits(div n 10), [mod n 10]] 

-- toDigitsRev :: Integer -> [Integer]
-- toDigitsRev n

reverseList :: [a] -> [a]
reverseList [] = []
reverseList [x] = [x]
reverseList (x:xs) = concat [reverseList(xs), [x]]
