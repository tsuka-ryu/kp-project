import Data.Array (Array, accumArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad (forM_)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

graph :: Int -> [[Int]] -> Array Int [Int]
graph n uvs = accumArray (flip (:)) [] (1, n) xs
  where
    xs = concatMap (\[u, v] -> [(u, v)]) uvs -- 双方向に値を格納しなくて良さそう
    -- xs = concatMap (\[u, v] -> [(u, v), (v, u)]) uvs

main :: IO ()
main = do
  [n] <- getInts
  as <- getInts
  let xs = map (\(p, q) -> [p, q]) $ zip as [2 :: Int ..]
      g = graph n xs
  forM_ g $ \x -> do
    print $ length x
