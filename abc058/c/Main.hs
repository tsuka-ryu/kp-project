import Control.Monad (replicateM)
import Data.List (foldl1', intersect, sort)

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n  getLine
  let as = map sort ss
  let xs = foldl1' intersect as
  -- print ss
  putStrLn xs
