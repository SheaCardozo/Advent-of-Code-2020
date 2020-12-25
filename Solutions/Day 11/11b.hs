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
adjEmpt state r c = '#' `notElem` seats
    where seats = seatsSeen state r c (filter (/= (0, 0)) [(i, j) | i <- [-1..1], j <- [-1..1]])

adjFull :: [String] -> Int -> Int -> Bool
adjFull state r c = sum ([if s == '#' then 1 else 0| s <- seats]) >= 5
    where seats = seatsSeen state r c (filter (/= (0, 0)) [(i, j) | i <- [-1..1], j <- [-1..1]])

seatsSeen :: [String] -> Int -> Int -> [(Int, Int)] -> [Char]
seatsSeen state r c = map (seatSeen state r c 1) 

seatSeen :: [String] -> Int -> Int -> Int -> (Int, Int) -> Char
seatSeen state r c t dir 
    | valInds state (r + t * fst dir) (c + t * snd dir) = if state !! (r + t * fst dir) !! (c + t * snd dir) == '.' then seatSeen state r c (t + 1) dir else state !! (r + t * fst dir) !! (c + t * snd dir)
    | otherwise = '.'

valInds :: [String] -> Int -> Int -> Bool
valInds state r c = r >= 0 && c >= 0 && r < length state && c < (length . head $ state) 


compareState :: [String] -> [String] -> Bool
compareState a b = all (==True) (zipWith (==) a b) 

occupiedSeats :: [String] -> Int
occupiedSeats state = sum (map (length . filter (=='#')) state)