import Data.List as L

main :: IO ()
main = do
  _ <- readLn :: IO Int
  s <- getLine
  print $ length $ L.group s
