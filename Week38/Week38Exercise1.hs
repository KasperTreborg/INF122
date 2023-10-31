module Week38Exercise1 where

combinations :: Integer -> [Char] -> [String]
combinations 0 _ = [""]
combinations i cha = [x : xs | x <- cha,
                            xs <- combinations (i-1) cha]