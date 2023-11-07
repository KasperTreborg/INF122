{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Week43Exercise1 where

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Show)

instance Functor RoseTree where
  fmap f (Branch a subtrees) = Branch (f a) (map (fmap f) subtrees)

productNodes :: (Num a) => RoseTree [a] -> RoseTree a
productNodes = fmap product