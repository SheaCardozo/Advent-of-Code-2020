module Main where

import Data.List.Split (splitOn)

filename :: FilePath
filename = "Solutions/Day 13/13.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents

result :: [String] -> Integer 
result content = compute (splitOn "," . head . tail $ content)

compute :: [String] -> Integer 
compute seq = loop (read . head $ seq) (read . head $ seq) 1 (tail seq)

loop :: Integer -> Integer -> Integer -> [String] -> Integer
loop curr jump offset seq 
    | null seq = curr
    | head seq == "x" = loop curr jump (offset + 1) (tail seq)
    | (curr + offset) `mod` (read . head $ seq) == 0 = loop curr (jump * (read . head $ seq)) (offset + 1) (tail seq)
    | otherwise = loop (curr + jump) jump offset seq