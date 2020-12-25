module Main where

filename :: FilePath
filename = "Solutions/Day 12/12.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents

result :: [String] -> Int
result content = (abs . fst $ end) + (abs . snd $ end)
    where end = runCommand (0, 0) 'E' content

runCommand :: (Int, Int) -> Char -> [String] -> (Int, Int)
runCommand (x, y) o commands = do
    if null commands then
        (x, y)
    else do
        let c = head commands
        let p = head c
        let n = read . tail $ c :: Int
        if p `elem` ['N', 'E', 'S', 'W'] then
            runCommand (moveShip (x, y) p n) o (tail commands) 
        else do
            if p == 'F' then
                runCommand (moveShip (x, y) o n) o (tail commands) 
            else 
                runCommand (x, y) (rotateShip o p n) (tail commands) 

moveShip :: (Int, Int) -> Char -> Int -> (Int, Int)
moveShip (x, y) p n 
    | p == 'N' = (x + n, y)
    | p == 'E' = (x, y + n)
    | p == 'S' = (x - n, y)
    | p == 'W' = (x, y - n)

rotateShip :: Char -> Char -> Int -> Char
rotateShip o p n 
    | p == 'R' = degToDir (dirToDeg o + n)
    | p == 'L' = degToDir (dirToDeg o - n)

degToDir :: Int -> Char 
degToDir n 
    | n `mod` 360 == 0 = 'N'
    | n `mod` 360 == 90 = 'E'
    | n `mod` 360 == 180 = 'S'
    | n `mod` 360 == 270 = 'W'

dirToDeg :: Char -> Int 
dirToDeg o 
    | o == 'N' = 0
    | o == 'E' = 90
    | o == 'S' = 180
    | o == 'W' = 270
