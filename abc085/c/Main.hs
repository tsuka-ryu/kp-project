main :: IO ()
main = do
  [n, y] <- map read . words <$> getLine :: IO [Int]
  let xs = [(a, b, n - a - b) | a <- [0 .. n], b <- [0 .. n], a + b <= n, y == a * 10000 + b * 5000 + (n - a - b) * 1000]
  let res = take 1 xs
  let ys = (-1 :: Int, -1 :: Int, -1 :: Int)
  putStrLn $ if null res then tapleToString ys else tapleToString (res !! 0)

tapleToString (x, y, z) = show x ++ " " ++ show y ++ " " ++ show z