module Main where

import Data.Strings (strSplitAll, strReplace, strSplit, strReplace)
import Data.Map (Map, insert, empty, member, findWithDefault, size)
import Data.List (elemIndices)

-- Break up input for easier solution processing
rulesname :: FilePath
rulesname = "Solutions/Day 16/rules.txt"

ticketsname :: FilePath
ticketsname = "Solutions/Day 16/tickets.txt"

minename :: FilePath
minename = "Solutions/Day 16/my.txt"

main :: IO ()
main = do
    rawrules <- readFile rulesname
    tickets <- readFile ticketsname
    mine <- readFile minename
    let rules = map buildRule . lines $ rawrules
    let valid = validTickets (lines tickets) rules
    let myticket = map read (strSplitAll "," mine)
    let fieldmap = createFieldMap (map extName (lines rawrules)) rules 0 (map (map read . strSplitAll ",") valid) empty
    print (myticket !! findWithDefault (-1) "departure location" fieldmap * myticket !! findWithDefault (-1) "departure station" fieldmap *
           myticket !! findWithDefault (-1) "departure platform" fieldmap * myticket !! findWithDefault (-1) "departure track" fieldmap *
           myticket !! findWithDefault (-1) "departure date" fieldmap * myticket !! findWithDefault (-1) "departure time" fieldmap)

validTickets :: [String] -> [Int -> Bool] -> [String]
validTickets tickets rules = filter (checkRules rules) tickets

checkRules :: [Int -> Bool] -> String -> Bool
checkRules rules s = all (checkRule rules) fields 
    where fields = map read (strSplitAll "," s)

checkRule :: [Int -> Bool] -> Int -> Bool
checkRule rules field = any (\f -> f field) rules

buildRule :: String -> Int -> Bool 
buildRule rule = \i -> ((i >= head bounds) && (i <= (head . tail $ bounds))) || ((i >= (head . tail . tail $ bounds)) && (i <= (head . tail . tail . tail $ bounds)))
    where bounds = map read (strSplitAll "-" (strReplace " or " "-" (snd $ strSplit ": " rule)))

extName :: String -> String
extName s = fst $ strSplit ": " s

createFieldMap :: [String] -> [Int -> Bool] -> Int -> [[Int]] -> Map String Int -> Map String Int
createFieldMap names rules i tickets fmap
    | size fmap == length names = fmap
    | length nind /= 1 || member (names !! head nind) fmap = createFieldMap names rules ((i+1) `mod` length names) tickets fmap
    | otherwise = createFieldMap names (map (updateRules rules (head nind)) [0..(length rules - 1)]) ((i+1) `mod` length names) tickets (insert (names !! head nind) i fmap)
    where nind = elemIndices True (map (\f -> all f [t !! i | t <- tickets]) rules)

updateRules :: [Int -> Bool] -> Int -> Int -> Int -> Bool
updateRules rules ind i 
    | ind == i = const False 
    | otherwise = rules !! i 