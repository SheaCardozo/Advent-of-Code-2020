module Main where

filename :: FilePath
filename = "Solutions/Day 1/1.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result :: [String] -> Int
result input = head $ filter (/= 0) [ yearCheck x y (read i) (read j) 
                | (x, i) <- zip [1..] input, (y, j) <- zip [1..] input ]

yearCheck :: Int -> Int -> Int -> Int -> Int
yearCheck x y i j 
        | x == y = 0
        | i + j == 2020 = i * j
        | otherwise = 0