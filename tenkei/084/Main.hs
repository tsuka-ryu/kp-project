import Data.List qualified as L

main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- getLine
  let all' = (n * (n + 1)) `div` 2
  let ret = map ((\x -> (x * (x + 1)) `div` 2) . length) $ L.group s
  print $ all' - sum ret
