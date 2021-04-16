module Main where

import Lib (jacobi)
import Data.List (intercalate)

-- Сумма символов Якоби на отрезке последовательно идущих чисел
jacobiRangeSum :: 
  Integer -> -- Начало отрезка
  Int -> -- Длина отрезка
  Integer -> -- Простое число p
  Integer
jacobiRangeSum range_start range_length prime =
  sum [jacobi x prime | x <- take range_length [range_start..]]

quarterRangeStarts ::
  Int -> -- Длина отрезка
  Integer -> -- Простое число p
  [Integer]
quarterRangeStarts range_length prime =
  [ 1
  , prime - toInteger range_length
  , div prime 4
  , div prime 4 - toInteger range_length
  , div prime 2
  , div prime 2 - toInteger range_length
  , div prime 4 * 3
  , div prime 4 * 3 - toInteger range_length
  ]

hints :: [String]
hints = 
  [ "1 (вправо): "
  , "1 (влево): "
  , "p/4 (вправо): "
  , "p/4 (влево): "
  , "p/2 (вправо): "
  , "p/2 (влево): "
  , "3p/4 (вправо): "
  , "3p/4 (влево): "
  ]

rangeReport ::
  Integer -> -- Начало отрезка
  Int -> -- Длина отрезка
  Integer -> -- Простое число p
  String
rangeReport range_start range_length prime =
  "Сумма на отрезке [" ++ 
    show range_start ++ "; " ++ show (range_start + toInteger range_length - 1) ++
    "]: " ++ show (jacobiRangeSum range_start range_length prime)

report ::
  Int -> -- Длина отрезка
  Integer -> -- Простое число p
  String
report range_length prime =
  "Длина отрезка: " ++ show range_length ++ ".\n" ++
  "Простое число p: " ++ show prime ++ ".\n" ++ 
  intercalate "\n" [rangeReport range_start range_length prime
                   | range_start <- quarterRangeStarts range_length prime
                   ]

bigPrime_10_12 :: Integer
bigPrime_10_12 = 1000000000039

main :: IO ()
main = putStrLn $ report 2 bigPrime_10_12
