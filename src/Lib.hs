module Lib
    ( jacobi
    ) where

import Data.List (replicate, transpose)
import Data.Bool (bool)

jacobi :: Int -> Int -> Int
jacobi = go
  where
    go 0 1 = 1
    go 0 _ = 0
    go x y
      | even r = plusMinus (rem y 8 `elem` [3, 5]) (go (div r 2) y)
      | otherwise = plusMinus (p r && p y) (go y r)
      where
        plusMinus = bool id negate
        p = (3 ==) . flip rem 4
        r = rem x y