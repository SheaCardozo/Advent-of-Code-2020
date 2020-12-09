module Main where

import Lib ()

filename :: FilePath
filename = "Solutions/Day 3/3.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents

result ::  [String] -> Int
result =  ski 1 3

ski :: Int -> Int -> [String] -> Int
ski i j hill
    | i >= length hill = 0
    | (hill !! i) !! (j `mod` (length . head $ hill)) == '#' = 1 + ski (i+1) (j+3) hill
    | otherwise = ski (i+1) (j+3) hill
