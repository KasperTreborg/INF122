module Week41Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map

data Expr variable
  = Var variable
  | Lit Bool
  | And (Expr variable) (Expr variable)
  | Or (Expr variable) (Expr variable)
  deriving (Eq, Show)

eval :: (Ord variable) => Expr variable -> Map variable Bool -> Maybe Bool
eval (Var x) values = Map.lookup x values

eval (Lit b) _ = Just b

eval (And expr1 expr2) values = do
    val1 <- eval expr1 values
    val2 <- eval expr2 values
    Just (val1 && val2)

eval (Or expr1 expr2) values = do
    val1 <- eval expr1 values
    val2 <- eval expr2 values
    Just (val1 || val2)