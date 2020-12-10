module Main where

filename :: FilePath
filename = "Solutions/Day 8/8.txt"

-- *Technically* this doesn't work if the the program finishes executing with a accumulator value of 0
-- ...but it doesn't for my puzzle input, so ¯\_(ツ)_/¯

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result :: [String] -> Int
result content = do
    let inds = foldl (buildInds content) [] [0..length content - 1]
    let ins = map clean content

    head (filter (/= 0) (map (\i -> execute (convertIns ins i) [] 0 0) inds))

execute :: [[String]] -> [Int] -> Int -> Int -> Int
execute ins log acc stp 
    | stp >= length ins = acc
    | stp `elem` log = 0
    | head (ins !! stp) == "nop" = execute ins (stp : log) acc (stp + 1) 
    | head (ins !! stp) == "acc" = execute ins (stp : log) (acc + read (last (ins !! stp))) (stp + 1) 
    | head (ins !! stp) == "jmp" = execute ins (stp : log) acc (stp + read (last (ins !! stp))) 

clean :: String -> [String]
clean s = do
    let sp = words s
    let i = last sp
    init sp ++ if head i == '+' then [tail i] else [i]

buildInds :: [String] -> [Int] -> Int -> [Int]
buildInds ins inds i 
    | take 3 (ins !! i) == "nop" || take 3 (ins !! i) == "jmp" = i : inds
    | otherwise = inds

swapOp :: String -> String
swapOp s 
    | s == "nop" = "jmp"
    | s == "jmp" = "nop"

convertIns :: [[String]] -> Int -> [[String]]
convertIns ins i = take i ins ++ [[swapOp . head $ k, last k]] ++ drop (i+1) ins
    where k = ins !! i