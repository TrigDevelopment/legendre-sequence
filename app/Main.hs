module Main where

import Lib (jacobi)
import Data.List (intercalate)
import Math.NumberTheory.Logarithms (integerLog2)

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

rangeReport ::
  Integer -> -- Начало отрезка
  Int -> -- Длина отрезка
  Integer -> -- Простое число p
  String
rangeReport range_start range_length prime =
  "Sum on range [" ++ 
    show range_start ++ "; " ++ show (range_start + toInteger range_length - 1) ++
    "]: " ++ show (jacobiRangeSum range_start range_length prime)

report ::
  Int -> -- Длина отрезка
  Integer -> -- Простое число p
  String
report range_length prime =
  "Range length: " ++ show range_length ++ ".\n" ++
  "Prime: " ++ show prime ++ ".\n" ++ 
  intercalate "\n" [rangeReport range_start range_length prime
                   | range_start <- quarterRangeStarts range_length prime
                   ]

bigPrime_10_12 :: Integer
bigPrime_10_12 = 1000000000039

bigPrime_10_16 :: Integer
bigPrime_10_16 = 1000000000100011

bigPrime_10_20 :: Integer
bigPrime_10_20 = 10089886811898868001

bigPrime_10_50 :: Integer
bigPrime_10_50 = 74697529992376396975340788410411701659416034912859

rangeLength :: Integer -> Int
rangeLength prime = l * integerLog2 (toInteger l)
  where
    l = integerLog2 prime

fastReport :: Integer -> String
fastReport prime = report (rangeLength prime) prime

ranges :: Integer -> Integer -> Integer -> IO ()
ranges prime n_ranges start = writeFile name content
  where
    name = show n_ranges ++ " ranges starting from " ++ 
      show start ++ " for prime " ++ show prime ++ 
      " with length " ++ show range_length ++ ".csv"
    content = intercalate "\n" $ 
                [ show n ++ ";" ++ show (jacobiRangeSum n range_length prime)
                | n <- [start..(start + n_ranges - 1)]
                ]
    range_length = rangeLength prime

main :: IO ()
main = ranges bigPrime_10_50 (10^5) (div bigPrime_10_50 4)
-- main = writeFile "output.txt" $ fastReport bigPrime_10_50
