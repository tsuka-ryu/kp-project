import Data.Array.Unboxed
import Data.List (foldl1')

main :: IO ()
main = do
  [n, p, q] <- map read . words <$> getLine :: IO [Int]
  as <- map read . words <$> getLine :: IO [Int]

  let as' = listArray (1, n) as :: (UArray Int Int)

  let ix = [(i, j, k, l, m) | i <- [1 .. n], j <- [i + 1 .. n], k <- [j + 1 .. n], l <- [k + 1 .. n], m <- [l + 1 .. n], let v = [as' ! i, as' ! j, as' ! k, as' ! l, as' ! m], let q' = foldl1' (\ !a !b -> (a * b) `rem` p) v, q' == q]
  print $ length ix
