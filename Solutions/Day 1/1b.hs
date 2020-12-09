module Main where

import Lib ()

filename :: FilePath
filename = "Solutions/Day 1/1.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result ::  [String] -> Int
result input = head $ filter (/= 0) [ yearCheck x y z (read i) (read j) (read k) 
                | (x, i) <- zip [1..] input, (y, j) <- zip [1..] input , (z, k) <- zip [1..] input]

yearCheck :: Int -> Int -> Int -> Int -> Int -> Int -> Int
yearCheck x y z i j k
        | x == y || x == z || y == z = 0
        | i + j + k == 2020 = i * j * k
        | otherwise = 0