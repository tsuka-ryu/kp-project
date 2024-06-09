import Control.Monad (forM_, replicateM)
import Data.Array (Array, accumArray, (!))
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

graph :: Int -> [[Int]] -> Array Int [Int]
graph n uvs = accumArray (flip (:)) [] (1, n) xs
  where
    xs = concatMap (\[u, v] -> [(u, v), (v, u)]) uvs

main :: IO ()
main = do
  [n, m, q] <- getInts
  uvs <- replicateM m getInts
  cs <- getInts
  qs <- replicateM q getInts

  cs' <- newListArray @IOArray (1, n) cs
  let g = graph n uvs
  print g

  forM_ qs $ \query -> do
    case query of
      [1, x] -> do
        v <- readArray cs' x
        mapM_ (\x -> writeArray cs' x v) $ g ! x
        print v
      [2, x, y] -> do
        v <- readArray cs' x
        writeArray cs' x y
        print v
      _ -> error "invalid query"

-- 写経