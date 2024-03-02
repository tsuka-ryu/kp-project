{-# OPTIONS_GHC -Wno-type-defaults #-}

import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

main :: IO ()
main = do
  [n] <- getInts
  xs <- getInts
  let ans = sum [if isTriangle (xs !! i) (xs !! j) (xs !! k) then 1 else 0 | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], k <- [j + 1 .. n - 1]]
  print ans

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

isTriangle :: Int -> Int -> Int -> Bool
isTriangle x y z = x /= y && x /= z && y /= z && x + y > z && x + z > y && y + z > x