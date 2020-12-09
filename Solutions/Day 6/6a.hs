module Main where

import Data.List (nub)
import Data.List.Split (splitOn)

filename :: FilePath
filename = "Solutions/Day 6/6.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result ::  [String] -> Int
result content = foldl groupAsk 0 (splitOn [""] content)

groupAsk :: Int -> [String] -> Int
groupAsk c group = c + qAnswered group

qAnswered :: [String] -> Int
qAnswered = length . nub . concat 