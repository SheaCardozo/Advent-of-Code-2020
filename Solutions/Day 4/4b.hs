module Main where

import Data.Char ( isDigit )

filename :: FilePath
filename = "Solutions/Day 4/4.txt"


required :: [String]
required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

ecl :: [String]
ecl = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

hcCheck :: Char -> Bool
hcCheck x = isDigit x || x `elem` "abcdef"

isInt :: String -> Bool
isInt x = not (null x) && all (==True) [isDigit i | i <- x]

valfuns :: [String -> Bool]
valfuns = [\x -> isInt x && read x >= 1920 && read x <= 2002, 
           \x -> isInt x && read x >= 2010 && read x <= 2020, 
           \x -> isInt x && read x >= 2020 && read x <= 2030, 
           \x -> if (reverse . take 2 . reverse $ x) == "in" then isInt (init . init $ x) && read (init . init $ x) >= 59 && read (init . init $ x) <= 76 else isInt (init . init $ x) && read (init . init $ x) >= 150 && read (init . init $ x) <= 193,
           \x -> (head x == '#') && all ((== True) . hcCheck) (tail x),
           (`elem` ecl),
           \x -> length x == 9 && isInt x]

passSplit :: [String] -> [String]
passSplit = foldl (\x y -> 
    if y == "" 
    then "" : (tail . head $ x) : tail x
    else (" " ++ y ++ head x) : tail x) [""]

passWrap :: [String] -> [String]
passWrap content = do
    let spt = passSplit content
    (tail . head $ spt) : tail spt

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents


result :: [String] -> Int
result content = sum (map validPass (passWrap content))

validPass :: String -> Int
validPass pass 
    | all ((== True) . fieldPres (words pass)) [0..(length required - 1)] = 1
    | otherwise = 0

fieldPres :: [String] -> Int -> Bool
fieldPres fields p = any ((== True) . isField p) fields

isField :: Int -> String -> Bool
isField a b 
        | (required !! a) == take 3 b = valField a (drop 4 b)
        | otherwise = False

valField :: Int -> String -> Bool
valField a = valfuns !! a