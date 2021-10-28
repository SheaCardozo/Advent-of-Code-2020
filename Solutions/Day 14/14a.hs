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
    let address = extNum . head $ splitOn "=" input
    let value = extNum . head . tail $ splitOn "=" input
    insert address (maskVal mask value) mem

extNum :: String -> String 
extNum s 
    | null s = s
    | isDigit (head s) = head s : (extNum . tail $ s)
    | otherwise = extNum . tail $ s

maskVal :: String -> String -> Int
maskVal mask val = do
    let vbin = showIntAtBase 2 intToDigit (read val) ""
    let evbin = ['0' | x <- [1..(length mask - length vbin)]] ++ vbin
    let masked = combM mask evbin
    toDec masked

combM :: String -> String -> String
combM mask evbin 
    | null mask = ""
    | head mask == 'X' = head evbin : combM (tail mask) (tail evbin)
    | otherwise = head mask : combM (tail mask) (tail evbin)
    where x = print mask

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0
