module Main where

filename :: FilePath
filename = "Solutions/Day 11/11.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result :: [String] -> Int
result content = do
    let next = updateState content
    if compareState content next then 
        occupiedSeats next 
    else 
        result next

updateState :: [String] -> [String]
updateState state = map (updateRow state) inds
    where inds = [0..93]

updateRow :: [String] -> Int -> String
updateRow state r = map (updateSeat state r) inds
    where inds = [0..91]

updateSeat :: [String] -> Int -> Int -> Char
updateSeat state r c 
    | (state !! r !! c == 'L') && adjEmpt state r c = '#'
    | (state !! r !! c == '#') && adjFull state r c = 'L'
    | otherwise = state !! r !! c

adjEmpt :: [String] -> Int -> Int -> Bool
adjEmpt state r c = do
    let off = [-1..1]
    let inds = filter (/= (-1, -1)) [if valInds state (r+i) (c+j) && not (i == 0 && j == i) then (r + i, c + j) else (-1, -1) | i <- off, j <- off]
    '#' `notElem` ([state !! i !! j | (i, j) <- inds])

adjFull :: [String] -> Int -> Int -> Bool
adjFull state r c = do
    let off = [-1..1]
    let inds = filter (/= (-1, -1)) [if valInds state (r+i) (c+j) && not (i == 0 && j == i) then (r + i, c + j) else (-1, -1) | i <- off, j <- off]
    sum ([if state !! i !! j == '#' then 1 else 0| (i, j) <- inds]) >= 4

valInds :: [String] -> Int -> Int -> Bool
valInds state r c = r >= 0 && c >= 0 && r < length state && c < (length . head $ state) 


compareState :: [String] -> [String] -> Bool
compareState a b = all (==True) (zipWith (==) a b) 

occupiedSeats :: [String] -> Int
occupiedSeats state = sum (map (length . filter (=='#')) state)