module Main where

filename :: FilePath
filename = "Solutions/Day 12/12.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents

result :: [String] -> Int
result content = (abs . fst $ end) + (abs . snd $ end)
    where end = runCommand (0, 0) (1, 10) content

runCommand :: (Int, Int) -> (Int, Int) -> [String] -> (Int, Int)
runCommand (x, y) (wx, wy) commands = do
    if null commands then
        (x, y)
    else do
        let c = head commands
        let p = head c
        let n = read . tail $ c :: Int
        if p `elem` ['N', 'E', 'S', 'W'] then
            runCommand (x, y) (moveWaypoint (wx, wy) p n) (tail commands) 
        else do
            if p == 'F' then
                runCommand (x + n * wx, y + n * wy) (wx, wy) (tail commands) 
            else 
                runCommand (x, y) (rotateShip (wx, wy) p n) (tail commands) 

moveWaypoint :: (Int, Int) -> Char -> Int -> (Int, Int)
moveWaypoint (x, y) p n 
    | p == 'N' = (x + n, y)
    | p == 'E' = (x, y + n)
    | p == 'S' = (x - n, y)
    | p == 'W' = (x, y - n)

rotateShip :: (Int, Int) -> Char -> Int -> (Int, Int)
rotateShip (wx, wy) p n 
    | p == 'R' = turnRight (n `div` 90) (wx, wy)
    | p == 'L' = turnLeft (n `div` 90) (wx, wy)

turnRight :: Int -> (Int, Int) -> (Int, Int)
turnRight n (wx, wy) 
    | n == 0 = (wx, wy)
    | otherwise = turnRight (n - 1) (-wy, wx)

turnLeft :: Int -> (Int, Int) -> (Int, Int)
turnLeft n (wx, wy) 
    | n == 0 = (wx, wy)
    | otherwise = turnLeft (n - 1) (wy, -wx)
