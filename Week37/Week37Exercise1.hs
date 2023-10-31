module Week37Exercise1 where

semiFermat :: Integer -> Integer -> [(Integer, Integer, Integer)]
semiFermat n m
  = [(a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b], a^m + b^m == c^(m-1)]
