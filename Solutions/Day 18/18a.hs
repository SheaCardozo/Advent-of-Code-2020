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
    foldl procCmdStr 0 cmdstr
    

breakUpF ::  String -> Int -> String -> [String]
breakUpF rem bkts part  
    | null rem = [part]
    | head rem == '(' = breakUpF (tail rem) (bkts - 1) (head rem : part)
    | head rem == ')' = breakUpF (tail rem) (bkts + 1) (head rem : part)
    | bkts > 0 = breakUpF (tail rem) bkts (head rem : part)
    | head rem `elem` "+*" = breakUpF (tail rem) bkts "" ++ [head rem : part]
    | otherwise = breakUpF (tail rem) bkts (head rem : part)

procCmdStr :: Int -> String -> Int
procCmdStr run cmd 
    | head cmd == '*' = run * (next . tail $ cmd)
    | head cmd == '+' = run + (next . tail $ cmd)
    | otherwise = run + next cmd
    where next s =  if head s == '(' then compute (tail . init $ s) else read s