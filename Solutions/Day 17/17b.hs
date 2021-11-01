module Main where

import Data.Array (Array, array, listArray, elems, bounds, (!))


filename :: FilePath
filename = "Solutions/Day 17/17.txt"

stop :: Int
stop = 6

fsd :: (a, b, c, d) -> a
fsd (a, _, _, _) = a

scd :: (a, b, c, d) -> b
scd (_, b, _, _) = b

thd :: (a, b, c, d) -> c
thd (_, _, c, _) = c

ftd :: (a, b, c, d) -> d
ftd (_, _, _, d) = d

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents

result :: [String] -> Int
result content = game stop (createStart content)

createStart :: [String] -> Array (Int, Int, Int, Int) Int
createStart input = listArray ((0, 0, 0, 0), (0, 0, length input - 1, (length . head $ input) - 1)) [ if c == '#' then 1 else 0 | c <- concat input]


game :: Int -> Array (Int, Int, Int, Int) Int -> Int
game cycles state
      | cycles == 0 = sum . elems $ state
      | otherwise = game (cycles - 1) (updateState state)

updateState :: Array (Int, Int, Int, Int) Int -> Array (Int, Int, Int, Int) Int
updateState state = do
    let nbds = newBounds state
    array nbds [((w, z, y, x), updateTile state (w, z, y, x)) |  w <- [fsd . fst $ nbds.. fsd . snd $ nbds], 
                                                          z <- [scd . fst $ nbds.. scd . snd $ nbds],
                                                          y <- [thd . fst $ nbds.. thd . snd $ nbds],
                                                          x <- [ftd . fst $ nbds.. ftd . snd $ nbds]]

newBounds :: Array (Int, Int, Int, Int) Int -> ((Int, Int, Int, Int), (Int, Int, Int, Int))
newBounds state = do
    let b = bounds state
    let expw = if sum [state ! (w, z, y, x) | w <- [fsd . fst $ b, fsd . snd $ b], z <- [scd . fst $ b.. scd . snd $ b], y <- [thd . fst $ b.. thd . snd $ b], x <- [ftd . fst $ b.. ftd . snd $ b]] > 0 then 1 else 0
    let expz = if sum [state ! (w, z, y, x) | w <- [fsd . fst $ b.. fsd . snd $ b], z <- [scd . fst $ b, scd . snd $ b], y <- [thd . fst $ b.. thd . snd $ b], x <- [ftd . fst $ b.. ftd . snd $ b]] > 0 then 1 else 0 
    let expy = if sum [state ! (w, z, y, x) | w <- [fsd . fst $ b.. fsd . snd $ b], z <- [scd . fst $ b.. scd . snd $ b], y <- [thd . fst $ b, thd . snd $ b], x <- [ftd . fst $ b.. ftd . snd $ b]] > 0 then 1 else 0
    let expx = if sum [state ! (w, z, y, x) | w <- [fsd . fst $ b.. fsd . snd $ b], z <- [scd . fst $ b.. scd . snd $ b], y <- [thd . fst $ b.. thd . snd $ b], x <- [ftd . fst $ b, ftd . snd $ b]] > 0 then 1 else 0

    (((fsd . fst $ b) - expw, (scd . fst $ b) - expz, (thd . fst $ b) - expy, (ftd . fst $ b) - expx), ((fsd . snd $ b) + expw, (scd . snd $ b) + expz, (thd . snd $ b) + expy, (ftd . snd $ b) + expx))

updateTile :: Array (Int, Int, Int, Int) Int -> (Int, Int, Int, Int) -> Int 
updateTile state (w, z, y, x) 
    | nbrs == 3 = 1
    | nbrs == 2 && safeAccess state (w, z, y, x) == 1 = 1
    | otherwise = 0
    where nbrs = sum [safeAccess state (w + dw, z + dz, y + dy, x + dx) | dw <- [-1..1], dz <- [-1..1], dy <- [-1..1], dx <- [-1..1]] - safeAccess state (w, z, y, x)


safeAccess :: Array (Int, Int, Int, Int) Int -> (Int, Int, Int, Int) -> Int
safeAccess state (w, z, y, x)
    | and [w >= (fsd . fst $ b), w <= (fsd . snd $ b), z >= (scd . fst $ b), z <= (scd . snd $ b), y >= (thd . fst $ b), y <= (thd . snd $ b), x >= (ftd . fst $ b), x <= (ftd . snd $ b)] = state ! (w, z, y, x)
    | otherwise = 0
    where b = bounds state