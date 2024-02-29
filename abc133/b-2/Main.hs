{-# OPTIONS_GHC -Wno-type-defaults #-}

import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

distance :: [Int] -> [Int] -> Double
distance v1 v2 = sqrt . fromIntegral . sum $ zipWith (\y z -> (y - z) ^ (2 :: Int)) v1 v2

isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

main :: IO ()
main = do
  [n, _] <- getInts
  vs <- replicateM n getInts
  print vs
  print $
    [ (i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]
    ]

  print $
    sum
      [ if isInt d then 1 else 0
        | i <- [0 .. n - 1],
          j <- [i + 1 .. n - 1],
          let d = distance (vs !! i) (vs !! j)
      ]
