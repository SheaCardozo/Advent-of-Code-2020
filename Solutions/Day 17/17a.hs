module Main where

import Data.Array (Array, array, listArray, elems, bounds, (!))


filename :: FilePath
filename = "Solutions/Day 17/17.txt"

stop :: Int
stop = 6

fsd :: (a, b, c) -> a
fsd (a, _, _) = a

scd :: (a, b, c) -> b
scd (_, b, _) = b

thd :: (a, b, c) -> c
thd (_, _, c) = c

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents

result :: [String] -> Int
result content = game stop (createStart content)

createStart :: [String] -> Array (Int, Int, Int) Int
createStart input = listArray ((0, 0, 0), (0, length input - 1, (length . head $ input) - 1)) [ if c == '#' then 1 else 0 | c <- concat input]


game :: Int -> Array (Int, Int, Int) Int -> Int
game cycles state
      | cycles == 0 = sum . elems $ state
      | otherwise = game (cycles - 1) (updateState state)

updateState :: Array (Int, Int, Int) Int -> Array (Int, Int, Int) Int
updateState state = do
    let nbds = newBounds state
    array nbds [((z, y, x), updateTile state (z, y, x)) | z <- [fsd . fst $ nbds.. fsd . snd $ nbds],
                                                          y <- [scd . fst $ nbds.. scd . snd $ nbds],
                                                          x <- [thd . fst $ nbds.. thd . snd $ nbds]]

newBounds :: Array (Int, Int, Int) Int -> ((Int, Int, Int), (Int, Int, Int))
newBounds state = do
    let b = bounds state
    let expz = if sum [state ! (z, y, x) | z <- [fsd . fst $ b, fsd . snd $ b], y <- [scd . fst $ b.. scd . snd $ b], x <- [thd . fst $ b.. thd . snd $ b]] > 0 then 1 else 0
    let expy = if sum [state ! (z, y, x) | z <- [fsd . fst $ b.. fsd . snd $ b], y <- [scd . fst $ b, scd . snd $ b], x <- [thd . fst $ b.. thd . snd $ b]] > 0 then 1 else 0 
    let expx = if sum [state ! (z, y, x) | z <- [fsd . fst $ b.. fsd . snd $ b], y <- [scd . fst $ b.. scd . snd $ b], x <- [thd . fst $ b, thd . snd $ b]] > 0 then 1 else 0

    (((fsd . fst $ b) - expz, (scd . fst $ b) - expy, (thd . fst $ b) - expx), ((fsd . snd $ b) + expz, (scd . snd $ b) + expy, (thd . snd $ b) + expx))

updateTile :: Array (Int, Int, Int) Int -> (Int, Int, Int) -> Int 
updateTile state (z, y, x) 
    | nbrs == 3 = 1
    | nbrs == 2 && safeAccess state (z, y, x) == 1 = 1
    | otherwise = 0
    where nbrs = sum [safeAccess state (z + dz, y + dy, x + dx) | dz <- [-1..1], dy <- [-1..1], dx <- [-1..1]] - safeAccess state (z, y, x)


safeAccess :: Array (Int, Int, Int) Int -> (Int, Int, Int) -> Int
safeAccess state (z, y, x)
    | and [z >= (fsd . fst $ b), z <= (fsd . snd $ b), y >= (scd . fst $ b), y <= (scd . snd $ b), x >= (thd . fst $ b), x <= (thd . snd $ b)] = state ! (z, y, x)
    | otherwise = 0
    where b = bounds state