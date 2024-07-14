import Control.Monad (replicateM)
import Data.Array.IArray (Array, accumArray, (!))
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.IntSet qualified as IS
import Data.List (foldl', partition, unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

graph :: Int -> [[Int]] -> Array Int [Int]
graph n uvs = accumArray (flip (:)) [] (1, n) xs
  where
    xs = concatMap (\[u, v] -> [(u, v), (v, u)]) uvs

type Context = ([(Int, Bool)], IS.IntSet)

dfs :: (Int -> [Int]) -> Context -> (Int, Bool) -> Context
dfs next (path, visited) (v, color) = foldl' f ((v, color) : path, IS.insert v visited) (next v)
  where
    f :: Context -> Int -> Context
    f context u
      | IS.member u (snd context) = context
      | otherwise = dfs next context (u, not color)

main :: IO ()
main = do
  [n] <- getInts
  uvs <- replicateM (n - 1) getInts
  let g = graph n uvs
  -- print g
  -- print $ dfs (g !) ([], IS.empty) (1, True) -- pathには逆順で経路が格納されてる
  -- print $ dfs (g !) ([], IS.empty) (2, True)
  -- print $ dfs (g !) ([], IS.empty) (3, True)
  let (vs, _) = dfs (g !) ([], IS.empty) (1, True)
  let vs1 = filter snd vs
  let vs2 = filter (not . snd) vs
  print vs1
  print vs2
  let result = if length vs1 > length vs2 then take (n `div` 2) vs1 else take (n `div` 2) vs2

  putStrLn $ unwords . map (show . fst) $ result
