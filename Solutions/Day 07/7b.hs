module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.List ( isSuffixOf )

filename :: FilePath
filename = "Solutions/Day 7/7.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result :: [String] -> Int
result content = do
    let rules = genRules (filter (not . isSuffixOf "no other bags.") content)
    containBags rules "shiny gold" - 1

genRules :: [String] -> Map.Map String [String]
genRules = foldl addRule Map.empty 

addRule :: Map.Map String [String] -> String -> Map.Map String [String]
addRule m s = do
    let rule = splitOn " bags contain " s 
    let k = head rule
    let v = concat [replicate (read . head . words $ i) (unwords . init . tail . words $ i) | i <- splitOn ", " (init . last $ rule)]
    Map.insert k v m

containBags :: Map.Map String [String] -> String -> Int
containBags m k 
    | Map.notMember k m = 1
    | otherwise = 1 + sum (map (containBags m) (m Map.! k))