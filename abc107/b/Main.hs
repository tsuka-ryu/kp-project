import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.List (transpose)

main :: IO ()
main = do
  [h, w] <- map read . words <$> getLine :: IO [Int]
  grid <- getGrid h w
  -- print $ grid
  let xs = f h w grid
  let omittedRow = filter (not . null) $ map omitWhite xs
  let ys = transpose omittedRow
  let omittedCol = filter (not . null) $ map omitWhite ys
  let res = transpose omittedCol
  forM_ res $ \p -> do
    putStrLn p

getGrid :: Int -> Int -> IO (UArray (Int, Int) Char)
getGrid h w = listArray ((0, 0), (h - 1, w - 1)) . concatMap BS.unpack <$> replicateM h BS.getLine

f :: (IArray a1 b1, Ix a2, Ix b2, Num a2, Num b2, Enum a2, Enum b2) => a2 -> b2 -> a1 (a2, b2) b1 -> [[b1]]
f h w grid = map (\xs -> map (grid !) xs) [[(i, j) | j <- [0 .. w - 1]] | i <- [0 .. h - 1]]

omitWhite :: [Char] -> [Char]
omitWhite xs
  | all (== '.') xs = []
  | otherwise = xs