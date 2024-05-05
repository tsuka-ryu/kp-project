{-# LANGUAGE LambdaCase #-}

import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List qualified as L

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

main :: IO ()
main = do
  [n, m] <- ints
  sc <- replicateM m ints2
  let xs = [(i, j, k) | i <- [0 :: Int .. 9], j <- [0 :: Int .. 9], k <- [0 :: Int .. 9]]
  let ys = filter (check sc) xs
  print sc
  print $ take 10 xs
  print $ take 10 ys
  let ans = take 1 ys
  putStrLn $ if null ans then "-1" else show' $ head ans

check :: [(Int, Int)] -> (Int, Int, Int) -> Bool
check sc x = all (test x) sc
  where
    test (i, j, k) (s, c) = case s of
      1 -> i == c
      2 -> j == c
      3 -> k == c
      _ -> False

show' (i, j, k) = show i ++ show j ++ show k