module Main where

filename :: FilePath
filename = "Solutions/Day 4/4.txt"


required :: [String]
required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

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


result ::  [String] -> Int
result content = sum (map validPass (passWrap content))

validPass :: String -> Int
validPass pass 
    | all ((== True) . fieldPres (words pass)) required = 1
    | otherwise = 0

fieldPres :: [String] -> String -> Bool
fieldPres fields p = any ((== True) . isField p) fields

isField :: String -> String -> Bool
isField a b = a == take 3 b