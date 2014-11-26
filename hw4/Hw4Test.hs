-- HW4 Tests:

module Main where
import Hw4

main :: IO ()
main = do
    let test1 = [3,4,5,6,7,8,9,10]
    putStrLn "fun1 test:"
    if (fun1 test1 == fun1' test1) 
    then putStrLn "1. Pass!"
    else putStrLn "1. Fail!"
    
