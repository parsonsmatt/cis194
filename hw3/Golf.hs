-- Homework 3

-- Exercise 1:

skips :: [a] -> [[a]]
skips [] = []

-- Taken from @Nefrubyr on StackOverflow.com
every n xs = case drop (n-1) xs of
             (y:ys) -> y : every n ys
			 [] -> []
