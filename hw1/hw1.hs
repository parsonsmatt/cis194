-- Exercise 1:

toDigits :: Integer -> [Integer]
toDigits n
	| n <= 0    = []
	| n <= 9    = [n]
	| otherwise = concat [toDigits(div n 10), [mod n 10]] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverseList (toDigits n)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = concat [reverseList(xs), [x]]

-- Exercise 2:

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [n] = [n]
-- doubleEveryOther [n,m] = [(2*n),m]
doubleEveryOther (n:m:xs) = n:m*2:doubleEveryOther(xs)

