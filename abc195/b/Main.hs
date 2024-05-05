main :: IO ()
main = do
  [a, b, w] <- map read . words <$> getLine :: IO [Int]
  let xs = [i | i <- [1 .. 1000000], a * i <= 1000 * w && 1000 * w <= b * i]
  print xs
  putStrLn $ if null xs then "UNSATISFIABLE" else unwords $ map show [head xs, last xs]
