main :: IO ()
main = do
  [k, s] <- map read . words <$> getLine :: IO [Int]
  let xs = sum [1 | x <- [0 .. k], y <- [0 .. k], x + y <= s, s - x - y <= k]
  print xs
