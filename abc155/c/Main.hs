import Control.Monad (forM_, replicateM)
import Data.List (sort, sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)

countElements :: (Ord a) => [a] -> Map.Map a Int
countElements xs = Map.fromListWith (+) [(x, 1) | x <- xs]

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n getLine
  let xs = sortBy (comparing snd) (Map.toList $ countElements ss)
  let m = snd $ last xs
  let ys = sort . map fst $ filter (\(_, p) -> p == m) xs
  forM_ ys $ \y -> do
    putStrLn y