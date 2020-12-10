module Main where

filename :: FilePath
filename = "Solutions/Day 9/9.txt"

invalid :: Int
invalid = 10884537

main :: IO ()
main = do
    contents <- readFile filename
    print . result . map read . lines $ contents


result :: [Int] -> Int
result content = maximum range + minimum range
    where range = findRange content

findRange :: [Int] -> [Int]
findRange ins = head (filter (not . null) [checkRange ins i j | i <- [0..length ins - 1], j <- [0..length ins - 1]])

checkRange :: [Int] -> Int -> Int -> [Int]
checkRange ins i j 
    | i >= j = []
    | sum range == invalid = range
    | otherwise = []
    where range = take (j - i + 1) (drop i ins)