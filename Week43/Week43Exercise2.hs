{-# LANGUAGE FlexibleInstances #-}
module Week43Exercise2 where

class IntegerGraph g where
  emptyGraph :: g
  insertNode :: Integer -> g -> g
  insertEdge :: Integer -> Integer -> g -> g
  nodeInGraph :: Integer -> g -> Bool
  edgeInGraph :: Integer -> Integer -> g -> Bool

data MyGraph = MyGraph [(Integer, [Integer])]

instance IntegerGraph MyGraph where
  emptyGraph = MyGraph []
  insertNode x (MyGraph nodes) = MyGraph $ (x, []) : nodes
  insertEdge from to (MyGraph nodes) = MyGraph $ updateNodes from to nodes
    where
      updateNodes _ _ [] = [(from, [to]), (to, [])]
      updateNodes x y ((a, ys) : xs)
        | x == a = (a , y : ys) : updateNodes x y xs
        | otherwise = (a, ys) : updateNodes x y xs
  nodeInGraph x (MyGraph nodes) = elem x $ map fst nodes
  edgeInGraph from to (MyGraph nodes) = edgeExists from to nodes
    where
      edgeExists _ _ [] = False
      edgeExists from to ((x, ys) : xs)
        | from == x = elem to ys || edgeExists from to xs
        | otherwise = edgeExists from to xs

instance Show MyGraph where
  show (MyGraph nodes) = "MyGraph " ++ show nodes

graph :: (IntegerGraph g) => g
graph =
  let graph' = insertNode 1 (insertNode 5 (insertNode 3 (insertNode 6 (insertNode 8 emptyGraph))))
  in insertEdge 1 6 (insertEdge 1 8 (insertEdge 5 1 (insertEdge 5 8 (insertEdge 8 5 graph'))))