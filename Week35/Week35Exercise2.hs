module Week35Exercise2 where


palindrome :: [a] -> [a]
palindrome xs = xs ++ (reverse(xs)) 
