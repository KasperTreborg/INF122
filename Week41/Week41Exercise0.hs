module Week41Exercise0 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe

type Graph n = Map n (Set n)

data MyNode = A | B | C
  deriving (Eq,Ord,Show)

graph0 :: Graph MyNode
graph0 = Map.fromList [(A,Set.fromList [A,B])
                      ,(B,Set.fromList [C])
                      ,(C,Set.fromList [A])]

bridge :: (Ord n) => n -> n -> Graph n -> Graph n
bridge n1 n2 g =
  let
    pathExists = case path g n1 n2 of
      Just _  -> True
      Nothing -> False
    updatedGraph = if pathExists
      then g
      else Map.insertWith Set.union n1 (Set.singleton n2) $
           Map.insertWith Set.union n2 Set.empty g
  in
    updatedGraph


edge :: (Ord n) => Graph n -> n -> n -> Bool
edge g x y = case Map.lookup x g of
        Nothing -> False
        (Just edges) -> Set.member y edges

path :: (Ord n)
     => Graph n -> n -> n -> Maybe [n]
path g start end = path' g start end Set.empty

path' :: (Ord n) => Graph n -> n -> n -> Set n -> Maybe [n]
path' g start end visited
    | Set.member start visited = Nothing
    | start == end = Just []
    | otherwise = do
        let visited' = Set.insert start visited
        nexts <- Map.lookup start g
        listToMaybe
            $ mapMaybe
            (\next -> do
                pathCont <- path' g next end visited'
                Just (next:pathCont))
            (Set.toList nexts)