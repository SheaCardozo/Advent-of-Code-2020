module Main where

filename :: FilePath
filename = "Solutions/Day 5/5.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result :: [String] -> Int
result content = do
    let ids = map seatID content
    let fids = [minimum ids..maximum ids]
    head (filter (`notElem` ids) fids)


seatID :: String -> Int
seatID pass = rowNum (take 7 pass) * 8 + colNum (reverse . take 3 .reverse $ pass)


rowNum :: String -> Int
rowNum pass = head (foldl rowHalf [0..127] pass)

rowHalf :: [Int] -> Char -> [Int]
rowHalf seats i
    | i == 'F' = take (length seats `div` 2) seats
    | otherwise = reverse . take (length seats `div` 2) . reverse $ seats

colNum :: String -> Int 
colNum pass = head (foldl colHalf [0..7] pass)

colHalf :: [Int] -> Char -> [Int]
colHalf seats i
    | i == 'L' = take (length seats `div` 2) seats
    | otherwise = reverse . take (length seats `div` 2) . reverse $ seats


