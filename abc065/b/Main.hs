import Control.Monad (filterM, foldM, forM_, replicateM)
import Data.Array.IArray (Array, accumArray, elems, (!))
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Ix (Ix)
import Data.Maybe (fromJust)
import Data.Sequence (Seq (Empty, (:<|)), (|>))
import Data.Sequence qualified as Seq

main :: IO ()
main = do
  n <- getInt
  as <- replicateM n getInt
  let uvs = zip [1 ..] as
  let g = graph n uvs
  print g
  let dist = bfs (g !) (-1) (1, n) [1]
  print dist
  print $ dist ! 2

getInt :: IO Int
getInt = fst . fromJust . BS.readInt <$> BS.getLine

graph :: Int -> [(Int, Int)] -> Array Int [Int]
graph n uvs = accumArray (flip (:)) [] (1, n) xs
  where
    xs = concatMap (\(u, v) -> [(u, v)]) uvs

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
