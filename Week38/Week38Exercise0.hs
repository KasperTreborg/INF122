module Week38Exercise0 where

runningSum :: [Integer] -> [Integer]
runningSum [] = []
runningSum xs = runningSum' xs 0
  where
    runningSum' :: [Integer] -> Integer -> [Integer]
    runningSum' [] _ = []
    runningSum' (x:xs) num = (x + num) : runningSum' xs (x + num)