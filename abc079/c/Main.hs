main :: IO ()
main = do
  x <- readLn :: IO Int
  let (a, a') = x `divMod` 1000
      (b, b') = a' `divMod` 100
      (c, c') = b' `divMod` 10
      d = c'
  let ops = [(i, j, k) | i <- [0 :: Int .. 1], j <- [0 :: Int .. 1], k <- [0 :: Int .. 1]]
  let ans = filter (f [a, b, c, d]) ops
  putStrLn $ show' [a, b, c, d] $ head ans

f [a, b, c, d] (i, j, k) = (g k (g j (g i a b) c) d) == 7
f _ _ = False

g 0 arg1 arg2 = arg1 + arg2
g 1 arg1 arg2 = arg1 - arg2
g _ arg1 arg2 = arg1 + arg2 -- ここは通らない

show' [a, b, c, d] (i, j, k) = show a ++ (convert i) ++ show b ++ (convert j) ++ show c ++ (convert k) ++ show d ++ "=7"
  where
    convert 0 = "+"
    convert 1 = "-"
    convert _ = "+"