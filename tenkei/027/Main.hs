import Control.Monad (forM_, replicateM)
import Data.List (sort)
import Data.Map.Strict qualified as Map

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n getLine
  let xs = zip ss [1 .. n]
  let ys = Map.fromListWith (++) [(key, [value]) | (key, value) <- xs]
  let ans = sort $ map (last . snd) $ Map.toList ys
  forM_ ans $ \x -> do
    print x
