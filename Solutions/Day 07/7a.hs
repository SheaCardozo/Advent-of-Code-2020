module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.List ( isPrefixOf )

filename :: FilePath
filename = "Solutions/Day 7/7.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result :: [String] -> Int
result content = do
    let rules = genRules (filter (not . isPrefixOf "shiny gold bags contain ") content)
    sum (map (containsWrap rules) (Map.keys rules))

genRules :: [String] -> Map.Map String [String]
genRules = foldl addRule Map.empty 

addRule :: Map.Map String [String] -> String -> Map.Map String [String]
addRule m s = do
    let rule = splitOn " bags contain " s 
    let k = head rule
    let v = [unwords . init . words $ drop 2 i | i <- splitOn ", " (init . last $ rule)]
    Map.insert k v m

containsWrap :: Map.Map String [String] -> String -> Int
containsWrap m k 
    | containsShiny m k = 1
    | otherwise = 0

containsShiny :: Map.Map String [String] -> String -> Bool
containsShiny m k 
    | k == "shiny gold" = True
    | Map.notMember k m = False
    | any (containsShiny m) (m Map.! k) = True
    | otherwise = False
