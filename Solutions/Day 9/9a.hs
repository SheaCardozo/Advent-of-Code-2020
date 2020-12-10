module Main where

filename :: FilePath
filename = "Solutions/Day 9/9.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . map read . lines $ contents


result :: [Int] -> Int
result content = last (checkIns content 0 [] [])

checkIns :: [Int] -> Int -> [Int] -> [Int] -> [Int]
checkIns ins i buff r 
    | i >= length ins = r
    | length buff < 25 = checkIns ins (i+1) (k:buff) r
    | sumPast k buff = checkIns ins (i+1) (k:init buff) r
    | otherwise = checkIns ins (i+1) (k:init buff) (k:r)
    where k = ins !! i

sumPast :: Int -> [Int] -> Bool
sumPast i buff = i `elem` [if x /= y then x + y else -1 | x <- buff , y <- buff]