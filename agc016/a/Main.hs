import Debug.Trace (traceShow)

main :: IO ()
main = do
  s <- getLine
  print $ solve s

-- print $ filter (not . null) $ splitWhen (== 'a') "aaabbbbabbccdacdava"
-- print $ maxSegmentLength "aaabbbbabbccdacdava" 'a'

solve :: String -> Int
solve s = minimum ms
  where
    ms = map (maxSegmentLength s) s
    !_ = traceShow ms ()

maxSegmentLength :: String -> Char -> Int
maxSegmentLength s c = maximum . map length $ splitByChar s c

splitByChar :: String -> Char -> [String]
splitByChar s c = filter (not . null) $ splitWhen (== c) s

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs = l : splitWhen p xs'
  where
    (l, rest) = break p xs
    xs' = case rest of
      [] -> []
      (_ : xs'') -> xs''