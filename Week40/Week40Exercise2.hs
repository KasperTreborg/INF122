module Week40Exercise2 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

toBinarySearchTree :: Ord a => [a] -> BinSearchTree a
toBinarySearchTree [] = Empty
toBinarySearchTree xs =
  let len = length xs
      mid = len  `div` 2
      (left, x:right) = splitAt mid xs
      leftTree =  toBinarySearchTree left
      rightTree = toBinarySearchTree right
  in Branch leftTree x rightTree