import Control.Monad (replicateM)
import Data.Map.Strict qualified as Map

countElements :: (Ord a) => [a] -> Map.Map a Int
countElements xs = Map.fromListWith (+) [(x, 1) | x <- xs]

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- replicateM n getLine
  print . length . Map.keys . Map.filter odd $ countElements as
