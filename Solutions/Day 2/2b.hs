module Main where

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
    valCheck (decompCons . head $ wrds) (head (wrds !! 1)) (last wrds)

decompCons :: String -> (Int, Int)
decompCons cons = do
    let de = break ('-' ==) cons
    ((read . fst $ de) - 1, (read . tail . snd $ de) - 1)

valCheck :: (Int, Int) -> Char -> String -> Int
valCheck cons c pass
    | (pass !! fst cons == c || pass !! snd cons == c) && 
        pass !! fst cons /= pass !! snd cons = 1
    | otherwise = 0

            