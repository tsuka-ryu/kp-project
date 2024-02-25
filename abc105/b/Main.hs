main :: IO ()
main = do
  n <- readLn :: IO Int
  let xs = [x + y | x <- [0 :: Int, 4 .. 100], y <- [0, 7 .. 100]]
  putStrLn $ if n `elem` xs then "Yes" else "No"
