module Week36Exercise1 where

f :: [Integer] -> [t] -> [(Integer, t)]
f x = zip (reverse x)