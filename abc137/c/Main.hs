import Control.Monad (replicateM)
import Data.List (sort)
import Data.Map.Strict qualified as Map

sortString :: IO String
sortString = do
  str <- getLine :: IO String
  return $ sort str

countElements :: (Ord a) => [a] -> Map.Map a Int
countElements xs = Map.fromListWith (+) [(x, 1) | x <- xs]

main :: IO ()
main = do
  n <- readLn :: IO Int
  xs <- replicateM n sortString
  let ys = countElements xs
  let ans = sum $ map (\(_, n) -> n * (n - 1) `div` 2) $ Map.toList ys
  print ans

-- 解法
-- あらかじめソートする
-- 連想配列で数える
-- 組み合わせn(n-1)/2を合算する