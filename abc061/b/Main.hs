import Control.Monad (replicateM, forM_ )
import Data.Array (Array, accumArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

graph :: Int -> [[Int]] -> Array Int [Int]
graph n as = accumArray (flip (:)) [] (1, n) xs
  where
    xs = concatMap (\[u, v] -> [(u, v), (v, u)]) as

main :: IO ()
main = do
  [n, m] <- getInts
  as <- replicateM m getInts
  let g = graph n as
  forM_ g $ \x -> do
    print $ length x