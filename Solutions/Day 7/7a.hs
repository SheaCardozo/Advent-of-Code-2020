module Main where

import Data.List.Split (splitOn)

filename :: FilePath
filename = "Solutions/Day 7/7.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result ::  [String] -> Int
result content = 
