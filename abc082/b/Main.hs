import Data.List (sort)

main :: IO ()
main = do
  s <- sort <$> getLine
  t <- reverse . sort <$> getLine
  putStrLn $ if s < t then "Yes" else "No"