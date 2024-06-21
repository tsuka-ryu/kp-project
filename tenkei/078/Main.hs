import Control.Monad (replicateM)
import Data.Array.IArray
  ( Array,
    accumArray,
    assocs,
    elems,
    (!),
  )
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Set qualified as Set

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

graph :: Int -> [[Int]] -> Array Int [Int]
graph n uvs = accumArray (flip (:)) [] (1, n) xs
  where
    xs = concatMap (\[u, v] -> [(u, v), (v, u)]) uvs

main :: IO ()
main = do
  [n, m] <- getInts
  abs <- replicateM m getInts
  let g = graph n abs
  print $ length . filter id . map (\(i, xs) -> length (filter (< i) xs) == 1) $ assocs g