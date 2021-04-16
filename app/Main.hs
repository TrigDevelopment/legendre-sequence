module Main where

import Lib (jacobi)

main :: IO ()
main = putStrLn (show (jacobi 2 11))
