import Prelude hiding (elem)


data BalanceFactor = LeftHeavy
                   | Balanced
                   | RightHeavy
    deriving (Eq, Show)

data AVLTree a = Empty
               | Branch BalanceFactor
                        (AVLTree a)
                        a
                        (AVLTree a)
  deriving (Eq, Show)


leaf :: a -> AVLTree a
leaf x = Branch Balanced Empty x Empty


elem :: (Ord a) => a -> AVLTree a -> Bool
elem x Empty = False
elem x (Branch _ less y greater)
  | x == y = True
  | x < y = elem x less
  | otherwise = elem x greater

insertSimple :: (Ord a) => a -> AVLTree a -> AVLTree a

rotateLeft :: AVLTree a -> AVLTree a
rotateLeft Empty = Empty
rotateLeft t@(Branch lesser a Empty) = t
rotateLeft (Branch lesser a (Branch between b greater))
   = Branch (Branch lesser a between) b greater


rotateRight :: AVLTree a -> AVLTree a
rotateRight Empty = Empty
rotateRight t@(Branch Empty a greater) = t
rotateRight (Branch (Branch lesser a between) b greater)
   = Branch lesser a (Branch between b greater)



