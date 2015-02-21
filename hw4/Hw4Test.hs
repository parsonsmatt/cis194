-- HW4 Tests:

module Main where
import Hw4

main :: IO ()
main = do
    let test1 = [3,4,5,6,7,8,9,10]
    let test2 = [7,5,9,123,345,4]
    putStrLn "fun1 test:"
    if (fun1 test1 == fun1' test1) 
        then putStrLn "1. Pass!"
        else putStrLn "1. Fail!"
    if (fun1 test2 == fun1' test2)
        then putStrLn "2. Pass!"
        else putStrLn "2. Fail!"
    putStrLn $ show $ fun1 test2
    putStrLn $ show $ fun1' test2 
