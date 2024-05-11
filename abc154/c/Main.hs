import Data.Set qualified as Set

main :: IO ()
main = do
  _ <- readLn :: IO Int
  xs <- map read . words <$> getLine :: IO [Int]
  putStrLn $ if length xs == length (Set.fromList xs) then "YES" else "NO"
