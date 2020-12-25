module Main where

import Data.List (sort)
import Data.List.Split (splitOn)

filename :: FilePath
filename = "Solutions/Day 10/10.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . map read . lines $ contents


result :: [Int] -> Int
result content = compute . sort $ (0 : content ++ [maximum content + 3])

compute :: [Int] -> Int
compute content = foldl (\x y -> x * trans y) 1 (intdList content)

intdList :: [Int] -> [Int]
intdList content = filter (> 1) (map length (splitOn [3] (zipWith (\x y -> abs (x - y)) (init content) (tail content))))

trans :: Int -> Int
trans y 
    | y == 2 = 2
    | y == 3 = 4
    | y == 4 = 7