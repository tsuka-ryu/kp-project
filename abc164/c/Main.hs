import Control.Monad (replicateM)
import Data.Set qualified as Set

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n getLine
  print $ length $ Set.fromList ss
