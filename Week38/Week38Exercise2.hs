module Week38Exercise2 where

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (Just x:xs) = x : removeNothing xs
removeNothing (Nothing:xs) = removeNothing xs