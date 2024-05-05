{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt, isSpace)
import Data.List qualified as L
import Debug.Trace (traceShow)

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
  let xs = f n
  let ys = filter (\x -> all (check x n) sc) xs
  print ys
  putStrLn $ if null ys then "-1" else show . head $ take 1 ys

check x n (s, c) = isValid && (digitToInt (str !! (s - 1)) == c)
  where
    str = show x
    isValid = s <= n

f n =
  case n of
    1 -> [0 :: Int .. 9]
    2 -> [10 .. 99]
    3 -> [100 .. 999]
    _ -> []