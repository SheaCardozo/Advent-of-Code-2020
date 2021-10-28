module Main where

import Numeric (showIntAtBase)
import Data.Char (isDigit, intToDigit, digitToInt)
import Data.Map (Map, insert, fromList, foldl', empty)
import Data.List.Split (splitOn)


filename :: FilePath
filename = "Solutions/Day 14/14.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents

result :: [String] -> Int
result content = foldl' (+) 0 (compute empty "" content)

compute :: Map String Int -> String -> [String] -> Map String Int
compute mem mask inputs 
    | null inputs = mem
    | input !! 1 == 'a' = compute mem (drop 7 input) (tail inputs)
    | otherwise = compute (asnMem mem input mask) mask (tail inputs)
    where input = head inputs

asnMem :: Map String Int -> String -> String -> Map String Int
asnMem mem input mask = do
    let address = toBin . read $ (extNum . head $ splitOn "=" input)
    let value = read (extNum . head . tail $ splitOn "=" input)
    foldl (insMem value) mem (maskVal mask address)

insMem :: Int -> Map String Int -> String -> Map String Int 
insMem value mem s = insert s value mem

extNum :: String -> String 
extNum s 
    | null s = s
    | isDigit (head s) = head s : (extNum . tail $ s)
    | otherwise = extNum . tail $ s

maskVal :: String -> String -> [String]
maskVal mask val 
    | null mask = [""]
    | head mask == '0' = map (head val :) rest
    | head mask == '1' = map (head mask :) rest
    | head mask == 'X' = map ('0' :) rest ++ map ('1' :) rest
    where rest = maskVal (tail mask) (tail val)

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

toBin :: Int -> String 
toBin v = do 
    let vbin = showIntAtBase 2 intToDigit v ""
    ['0' | x <- [1..(36 - length vbin)]] ++ vbin
