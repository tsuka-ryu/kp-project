import Data.Map.Strict qualified as Map

countElements :: (Ord a) => [a] -> Map.Map a Int
countElements xs = Map.fromListWith (+) [(x, 1) | x <- xs]

main :: IO ()
main = do
  _ <- readLn :: IO Int
  an <- map read . words <$> getLine :: IO [Int]
  let xs = sum . map (\(key, value) -> f key value) $ Map.toList $ countElements an
  print xs

f k v
  | k - v > 0 = v 
  | k - v <= 0 = v - k
  | otherwise = 0