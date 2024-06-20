import Control.Monad (replicateM)
import Data.Array.IArray (
  Array,
  accumArray,
  assocs,
  listArray,
  (!),
 )
import Data.Array.Unboxed (UArray)
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
  [n, m] <- getInts
  hs <- listArray @UArray (1, n) <$> getInts
  abs <- replicateM m getInts
  let g = graph n abs
  print $ length . filter (\(idx, xs) -> all (\x -> hs ! x < hs ! idx) xs) $ assocs g
