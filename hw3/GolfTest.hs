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
    putStrLn "Testing 'localMaxina'..."
    putStrLn ("Actual:\t" ++ show (localMaxima [2,9,5,6,1]))
    putStrLn ("Expected: [9,6]")
    putStrLn ("Actual:\t" ++ show (localMaxima [1,2,3,4,5]))
    putStrLn ("Expected: []")
    putStrLn $ show $ localMaxima [2,3,4,1,5]

