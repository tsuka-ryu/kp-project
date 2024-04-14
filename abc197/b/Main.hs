import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS

main :: IO ()
main = do
  [h, w, x, y] <- map read . words <$> getLine :: IO [Int]
  grid <- getGrid h w
  print $ f grid h w x y

getGrid :: Int -> Int -> IO (UArray (Int, Int) Char)
getGrid h w = listArray ((1, 1), (h, w)) . concatMap BS.unpack <$> replicateM h BS.getLine

f :: UArray (Int, Int) Char -> Int -> Int -> Int -> Int -> Int
f grid h w hx wy = sum [up, down, left, right, me]
  where
    up = length . filter (/= '#') . takeWhile (/= '#') $ map (\idx -> grid ! idx) $ reverse [(i, wy) | i <- [1 .. hx], i /= hx]
    down = length . filter (/= '#') . takeWhile (/= '#') $ map (\idx -> grid ! idx) [(i, wy) | i <- [hx .. h], i /= hx]
    left = length . filter (/= '#') . takeWhile (/= '#') $ map (\idx -> grid ! idx) $ reverse [(hx, j) | j <- [1 .. wy], j /= wy]
    right = length . filter (/= '#') . takeWhile (/= '#') $ map (\idx -> grid ! idx) [(hx, j) | j <- [wy .. w], j /= wy]
    me = length . filter (/= '#') $ [grid ! (hx, wy)]