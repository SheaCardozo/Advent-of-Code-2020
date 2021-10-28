module Main where

import Data.List.Split (splitOn)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

filename :: FilePath
filename = "Solutions/Day 13/13.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents

result :: [String] -> Int
result content = compute 1 (read . head $ content) (getIds . head . tail $ content)

getIds :: String -> [Int]
getIds s = map read $ filter (/= "x") (splitOn "," s)

compute :: Int -> Int -> [Int] -> Int
compute i a b 
    | 0 `elem` modlist = i * (b !! fromMaybe (-1) (elemIndex 0 modlist))
    | otherwise = compute (i + 1) a b
    where modlist = map (mod (a + i)) b
