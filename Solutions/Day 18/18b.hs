module Main where

filename :: FilePath
filename = "Solutions/Day 18/18.txt"

main :: IO ()
main = do
    contents <- readFile filename
    print . result . lines $ contents

result :: [String] -> Int
result content =  sum (map (\xs -> compute [ x | x <- xs, x /= ' ']) content)

compute :: String -> Int
compute seq = do
    let cmdstr = breakUpF (reverse seq) 0 "" 
    multUp (addUp True 0 cmdstr)

breakUpF ::  String -> Int -> String -> [String]
breakUpF rem bkts part  
    | null rem = [part]
    | head rem == '(' = breakUpF (tail rem) (bkts - 1) (head rem : part)
    | head rem == ')' = breakUpF (tail rem) (bkts + 1) (head rem : part)
    | bkts > 0 = breakUpF (tail rem) bkts (head rem : part)
    | head rem `elem` "+*" = breakUpF (tail rem) bkts "" ++ [[head rem], part]
    | otherwise = breakUpF (tail rem) bkts (head rem : part)


addUp :: Bool -> Int -> [String] -> [String]
addUp add run cmd 
    | null cmd = [show run]
    | (head . head $ cmd) == '+' = addUp True run (tail cmd)
    | (head . head $ cmd) == '*' = show run : addUp False 0 (tail cmd)
    | add = addUp False (run + (next . head $ cmd)) (tail cmd)
    | otherwise = addUp add (next . head $ cmd) (tail cmd)
    where next s =  if head s == '(' then compute (tail . init $ s) else read s

multUp :: [String] -> Int
multUp cmd 
    | null cmd = 1
    | (head . head $ cmd) == '*' = multUp (tail cmd)
    | otherwise = (next . head $ cmd) * multUp (tail cmd)
    where next s =  if head s == '(' then compute (tail . init $ s) else read s

