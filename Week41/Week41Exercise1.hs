module Week41Exercise1 where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Data.Maybe

type Graph node = Map node (Set node)

data MyNode = A | B | C
  deriving (Eq,Ord,Show)

graph0 :: Graph MyNode
graph0 = Map.fromList [(A,Set.fromList [A,B])
                      ,(B,Set.fromList [C])
                      ,(C,Set.fromList [A])]

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint set1 set2 = Set.null (Set.intersection set1 set2)

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle g start =
    case Map.lookup start g of
        Nothing -> False
        Just edges -> hasCycle' g start edges Set.empty

hasCycle' :: (Ord n) => Graph n -> n -> Set n -> Set n -> Bool
hasCycle' g current children visited
    | not (disjoint children visited) = True
    | otherwise =
        any (\next -> hasCycle' g next (getChildren g next) (Set.insert current visited)) (Set.toList children)

getChildren :: (Ord n) => Graph n -> n -> Set n
getChildren g node = fromMaybe Set.empty (Map.lookup node g)

edge :: (Ord node)
     => Graph node -> node -> node -> Bool
edge g x y = case Map.lookup x g of
       Nothing -> False
       (Just edges) -> Set.member y edges

path :: (Ord node)
     => Graph node -> node -> node -> Maybe [node]
path g start end = path' g start end Set.empty


path' :: (Ord node)
     => Graph node
     -> node -> node
     -> Set node
     -> Maybe [node]
path' g start end visited
  | Set.member start visited = Nothing
        -- We have reached a cycle
  | start == end = Just []
  | otherwise
    = do
       let visited' = Set.insert start visited
       nexts <- Map.lookup start g
       listToMaybe
         $ mapMaybe
            (\next -> do
               pathCont <- path' g next end visited'
               Just (next:pathCont))
            (Set.toList nexts)