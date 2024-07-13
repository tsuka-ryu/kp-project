import Control.Monad (filterM, foldM, forM_, replicateM)
import Data.Array.IArray (Array, accumArray, elems, (!))
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.Ix (Ix)
import Data.List (unfoldr)
import Data.Sequence (Seq (Empty, (:<|)), (|>))
import Data.Sequence qualified as Seq

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

graph :: Int -> [[Int]] -> Array Int [Int]
graph n uvs = accumArray (flip (:)) [] (1, n) xs
  where
    xs = concatMap (\[u, v] -> [(u, v)]) uvs

bfs :: (Ix v) => (v -> [v]) -> Int -> (v, v) -> [v] -> UArray v Int
bfs nextStates initial (l, u_) v0s = runSTUArray $ do
  dist <- newArray (l, u_) initial

  forM_ v0s $ \v0 -> do
    writeArray dist v0 0

  aux (Seq.fromList v0s) dist
  return dist
  where
    aux Empty _ = return ()
    aux (v :<| queue) dist = do
      d <- readArray dist v
      us <- filterM (fmap (== initial) . readArray dist) (nextStates v)

      queue' <-
        foldM
          ( \q u -> do
              writeArray dist u (d + 1)
              return $ q |> u
          )
          queue
          us

      aux queue' dist

main :: IO ()
main = do
  [n, m] <- getInts
  uvs <- replicateM m getInts
  let g = graph n uvs
  -- print uvs
  -- let dist1 = bfs (g !) (n + 1) (1, n) [1] -- n+1は無限大の代わり
  -- let dist2 = bfs (g !) (n + 1) (1, n) [2]
  -- let dist3 = bfs (g !) (n + 1) (1, n) [3]
  -- let dist4 = bfs (g !) (n + 1) (1, n) [4]
  -- print dist1
  -- print dist2
  -- print dist3
  -- print dist4
  -- distは始点からの距離で、到達できない場合はn+1になる
  let dists = map (\x -> filter (/= n + 1) . elems $ bfs (g !) (n + 1) (1, n) [x]) [1 .. n]
  let ans = length $ concat dists
  print ans
