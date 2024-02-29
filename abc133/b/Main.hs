import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.List qualified as L

main :: IO ()
main = do
  [n, d] <- ints
  mat <- getMatInt n d
  let xs = [(x, y) | x <- [0 .. n - 1], y <- [0 .. n - 1], x /= y, x < y]
  let res = filter isSquare $ map sum $ map (\(x, y) -> map (\p -> let sub = (mat ! (x, p) - mat ! (y, p)) in sub * sub) [0 .. d - 1]) xs
  -- print xs
  -- print mat
  -- print res
  print $ length res

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
getMatInt n d = listArray ((0, 0), (n - 1, d - 1)) . concat <$> replicateM n ints

isSquare :: Int -> Bool
isSquare n = floor sqrtN ^ 2 == n || ceiling sqrtN ^ 2 == n
  where
    sqrtN = sqrt $ fromIntegral n
