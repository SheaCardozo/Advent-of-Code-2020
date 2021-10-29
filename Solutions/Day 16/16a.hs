module Main where

import Data.Strings (strSplitAll, strReplace, strSplit, strReplace)

-- Break up input for easier solution processing
rulesname :: FilePath
rulesname = "Solutions/Day 16/rules.txt"

ticketsname :: FilePath
ticketsname = "Solutions/Day 16/tickets.txt"

main :: IO ()
main = do
    rules <- readFile rulesname
    tickets <- readFile ticketsname
    print $ result (lines tickets) (buildRules . lines $ rules)

result :: [String] -> [Int -> Bool] -> Int
result tickets rules = sum . map (sum . checkRules rules) $ tickets

checkRules :: [Int -> Bool] -> String -> [Int]
checkRules rules s = map (checkRule rules) fields 
    where fields = map read (strSplitAll "," s)

checkRule :: [Int -> Bool] -> Int -> Int
checkRule rules field 
    | check = 0
    | otherwise = field
    where check = any (\x -> x field) rules

buildRules :: [String] -> [Int -> Bool]
buildRules rules 
    | null rules = []
    | otherwise = buildRule (head rules) : buildRules (tail rules)

buildRule :: String -> Int -> Bool 
buildRule rule = \i -> ((i >= head bounds) && (i <= (head . tail $ bounds))) || ((i >= (head . tail . tail $ bounds)) && (i <= (head . tail . tail . tail $ bounds)))
    where bounds = map read (strSplitAll "-" (strReplace " or " "-" (snd $ strSplit ": " rule)))