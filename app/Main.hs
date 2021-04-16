module Main where

import Lib (jacobi)
import Data.List (intercalate)

bigPrime_10_12 :: Integer
bigPrime_10_12 = 1000000000039

-- Сумма символов Якоби на отрезке последовательно идущих чисел
jacobiRangeSum :: 
  Integer -> -- Начало отрезка
  Int -> -- Длина отрезка
  Integer -> -- Простое число p
  Integer
jacobiRangeSum range_start range_length prime =
  sum [jacobi x prime | x <- take range_length [range_start..]]

-- Сумма символов Якоби на отрезках
-- [1, range_length], [p - range_length - 1, p - 1], ...
jacobiRangesOnQuarters :: 
  Int -> -- Длина отрезка
  Integer -> -- Простое число p
  [Integer]
jacobiRangesOnQuarters range_length prime =
  [ jacobiRangeSum start range_length prime | start <- 
    [ 1
    , prime - toInteger range_length - 1
    , div prime 4
    , div prime 4 - toInteger range_length
    , div prime 2
    , div prime 2 - toInteger range_length
    , div prime 4 * 3
    , div prime 4 * 3 - toInteger range_length
    ]
  ]

hints :: [String]
hints = [ "1 (вправо): "
  , "1 (влево): "
  , "p/4 (вправо): "
  , "p/4 (влево): "
  , "p/2 (вправо): "
  , "p/2 (влево): "
  , "3p/4 (вправо): "
  , "3p/4 (влево): "
  ]

main :: IO ()
main = putStrLn $ 
  intercalate "\n" (map (\(a, b) -> a ++ b) $ zip
    hints
    (map show (jacobiRangesOnQuarters 2 bigPrime_10_12))
    )
