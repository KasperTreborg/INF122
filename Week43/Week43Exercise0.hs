module Week43Exercise0 where

data BinSearchTree a
  = Leaf a
  | LeftRightChildBranch (BinSearchTree a) a (BinSearchTree a)
  | LeftChildBranch (BinSearchTree a) a
  | RightChildBranch a (BinSearchTree a)
  deriving (Eq, Show)

instance Foldable BinSearchTree where
  foldr f z (Leaf a) = f a z
  foldr f z (LeftRightChildBranch leftTree a rightTree) =foldr f (f a (foldr f z rightTree)) leftTree
  foldr f z (LeftChildBranch leftTree a) = foldr f (f a z) leftTree
  foldr f z (RightChildBranch a rightTree) = f a (foldr f z rightTree)


toList :: BinSearchTree a -> [a]
toList = foldr (:) []