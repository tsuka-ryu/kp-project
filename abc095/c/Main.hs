main :: IO ()
main = do
  [a, b, c, x, y] <- map read . words <$> getLine :: IO [Int]
  let res = [a' * a + b' * b + ab * c | ab <- [0 .. 200001], let a' = max 0 (x - ab `div` 2), let b' = max 0 (y - ab `div` 2)]
  print $ minimum res