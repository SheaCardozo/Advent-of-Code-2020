module Main where

import Data.List.Split (splitOn)
import Data.Map (Map, insert, empty, member, findWithDefault)


filename :: FilePath
filename = "Solutions/Day 15/15.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . head . lines $ contents

result :: String -> Int
result content = compute . map read $ splitOn "," content

compute :: [Int] -> Int
compute seq = game 1 (head seq) empty (tail seq)

game :: Int -> Int -> Map Int Int -> [Int] -> Int
game i next mem start 
    | i == 2020 = next -- Part 2 has this set to 30000000... takes a couple of minutes to run but works
    | not . null $ start = game (i + 1) (head start) (insert next i mem) (tail start)
    | member next mem = game (i + 1) (i - findWithDefault (-1) next mem) (insert next i mem) start
    | otherwise = game (i + 1) 0 (insert next i mem) start