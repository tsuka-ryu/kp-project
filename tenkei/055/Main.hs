main :: IO ()
main = do
  [n, p, q] <- map read . words <$> getLine :: IO [Int]
  as <- map read . words <$> getLine :: IO [Int]
  let xs = [take (i + 5) (drop i as) | i <- [0 .. n - 5]]
  let ys = filter (\m -> m `mod` p == q) $ map product xs
  print $ length ys
