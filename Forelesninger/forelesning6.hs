module Forelesning6 where

pyt :: [(Integer, Integer, Integer)]
pyt = [(a,b,c) | a <- [1..200]
               , b <- [a..200]
               , c <- [b..600]
               , a*a + b*b == c*c ]



















