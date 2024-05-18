import Control.Monad (replicateM)
import Data.Map.Strict qualified as Map

f xs = Map.fromListWith (+) ([(x, 1) | x <- xs] ++ ys)
  where
    ys = [(i, 0) | i <- ['a' .. 'z']]

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n getLine
  let xs = map f ss
  let ys = [(i, minimum (map (\x -> x Map.! i) xs)) | i <- ['a' .. 'z']]
  let ans = concat $ map (\(i, j) -> replicate j i) ys
  -- print xs
  -- print ys
  putStrLn ans 