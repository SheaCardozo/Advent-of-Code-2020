module Main where

import Data.List (sort)

filename :: FilePath
filename = "Solutions/Day 10/10.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . map read . lines $ contents


result :: [Int] -> Int
result = compute . sort 

compute :: [Int] -> Int
compute content = getans (zipWith (\x y -> abs (x - y)) (0 : content) (content ++ [maximum content + 3]))

getans :: [Int] -> Int
getans content = (length . filter (== 1)) content * (length . filter (== 3)) content