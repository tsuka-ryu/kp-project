main :: IO ()
main = do
  a <- readLn :: IO Int
  let xs = [x * y | x <- [1 :: Int .. 9], y <- [1 .. 9]]
  putStrLn $ if a `elem` xs then "Yes" else "No"
