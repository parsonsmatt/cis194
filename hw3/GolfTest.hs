-- GolfTest

module Main where

import Golf

main :: IO ()

main = do
	putStrLn "Testing 'skips' function..."
	putStrLn "skips \"ABCD\": "
	putStrLn ("Actual:\t" ++ show (skips "ABCD"))
	putStrLn "Expected: ['ABCD', 'BD', 'C', 'D']"
	if (skips "ABCD" == ["ABCD","BD","C","D"])
		then putStrLn "ABCD Passed!"
		else putStrLn "ABCD Failed!"

