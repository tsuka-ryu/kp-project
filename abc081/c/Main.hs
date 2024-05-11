import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)

countElements :: (Ord a) => [a] -> Map.Map a Int
countElements xs = Map.fromListWith (+) [(x, 1) | x <- xs]

main :: IO ()
main = do
  [_, k] <- map read . words <$> getLine :: IO [Int]
  as <- map read . words <$> getLine :: IO [Int]
  let xs = sortBy (comparing snd) (Map.toList $ countElements as)
  let ys = take (length xs - k) xs
  let ans = sum $ map snd ys
  -- print xs
  -- print ys
  print ans
