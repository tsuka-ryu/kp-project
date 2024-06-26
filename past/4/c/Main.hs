import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS

main :: IO ()
main = do
  [h, w] <- map read . words <$> getLine :: IO [Int]
  grid <- getGrid h w
  -- print grid
  let xs = [f h w (i, j) | i <- [0 .. h - 1], j <- [0 .. w - 1]]
  -- print xs
  let res = map (map (map (grid !))) xs
  -- print res
  let ans = map (length . concatMap (filter (== '#'))) res
  -- print ans
  let splitAns = chunksOf w ans
  -- print splitAns
  forM_ splitAns $ \as -> do
    putStrLn $ concatMap show as

getGrid :: Int -> Int -> IO (UArray (Int, Int) Char)
getGrid h w = listArray ((0, 0), (h - 1, w - 1)) . concatMap BS.unpack <$> replicateM h BS.getLine

f :: (Ord p1, Ord p2, Num p1, Num p2) => p1 -> p2 -> (p1, p2) -> [[(p1, p2)]]
f h w (i, j) = filter (not . null) $ map (\xs -> filter (\(p, q) -> isValid p q) xs) [[(i - 1, j - 1), (i - 1, j), (i - 1, j + 1)], [(i, j - 1), (i, j), (i, j + 1)], [(i + 1, j - 1), (i + 1, j), (i + 1, j + 1)]]
  where
    isValid p q = p >= 0 && p <= h - 1 && q >= 0 && q <= w - 1

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
