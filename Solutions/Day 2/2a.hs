module Main where

import Lib ()

filename :: FilePath
filename = "Solutions/Day 2/2.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result ::  [String] -> Int
result = sum . map isValid 

isValid :: String -> Int
isValid input = do
    let wrds = words input
    valCheck (decompCons . head $ wrds) (count (head (wrds !! 1)) (last wrds))

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

decompCons :: String -> (Int, Int)
decompCons cons = do
    let de = break ('-' ==) cons
    (read . fst $ de, read . tail . snd $ de)

valCheck :: (Int, Int) -> Int -> Int
valCheck cons cnt 
    | fst cons <= cnt && snd cons >= cnt = 1
    | otherwise = 0

            