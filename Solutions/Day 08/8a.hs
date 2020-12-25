module Main where

filename :: FilePath
filename = "Solutions/Day 8/8.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result :: [String] -> Int
result content = execute (map clean content) [] 0 0

execute :: [[String]] -> [Int] -> Int -> Int -> Int
execute ins log acc stp 
    | stp `elem` log = acc
    | head (ins !! stp) == "nop" = execute ins (stp : log) acc (stp + 1) 
    | head (ins !! stp) == "acc" = execute ins (stp : log) (acc + read (last (ins !! stp))) (stp + 1) 
    | head (ins !! stp) == "jmp" = execute ins (stp : log) acc (stp + read (last (ins !! stp))) 

clean :: String -> [String]
clean s = do
    let sp = words s
    let i = last sp
    init sp ++ if head i == '+' then [tail i] else [i]