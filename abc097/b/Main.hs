main :: IO ()
main = do
  x <- readLn :: IO Int
  let xs = [c | b <- [1 :: Int .. 32], p <- [2 :: Int .. 10], let c = b ^ p, c <= x]
  print $ maximum xs
