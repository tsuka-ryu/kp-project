main :: IO ()
main = do
  [a, b, _, k] <- map read . words <$> getLine :: IO [Int]
  print $ if even k then a - b else b - a
