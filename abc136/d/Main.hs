main :: IO ()
main = do
  s <- getLine
  let result = solve s
  putStrLn $ unwords $ map show result

solve :: String -> [Int]
solve s = concatMap distribute $ splitOnLR s

distribute :: String -> [Int]
distribute xs
  | length xs == 1 = [0]
  | otherwise =
      let (rCount, lCount) = span (== 'R') xs
          rLen = length rCount
          lLen = length lCount
          rEnd = (rLen + 1) `div` 2 + lLen `div` 2
          lEnd = rLen `div` 2 + (lLen + 1) `div` 2
       in replicate (rLen - 1) 0 ++ [rEnd, lEnd] ++ replicate (lLen - 1) 0

splitOnLR :: String -> [String]
splitOnLR [] = []
splitOnLR s = splitOnLR' s []
  where
    splitOnLR' [] acc = [acc]
    splitOnLR' [x] acc = [acc ++ [x]]
    splitOnLR' (x : y : xs) acc
      | x == 'L' && y == 'R' = (acc ++ [x]) : splitOnLR' (y : xs) []
      | otherwise = splitOnLR' (y : xs) (acc ++ [x])