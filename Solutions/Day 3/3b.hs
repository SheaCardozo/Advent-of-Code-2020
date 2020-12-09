module Main where

import Lib ()

filename :: FilePath
filename = "Solutions/Day 3/3.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents

result ::  [String] -> Int
result content =  ski 1 1 content * ski 1 3 content * ski 1 5 content * ski 1 7 content * ski 2 1 content

ski :: Int -> Int -> [String] -> Int
ski d p s = do
        let dp = \i j hill -> case () of 
              _ | i >= length hill -> 0
                | (hill !! i) !! (j `mod` (length . head $ hill)) == '#' -> 1 + dp (i+d) (j+p) hill
                | otherwise -> dp (i+d) (j+p) hill

        dp d p s
        
