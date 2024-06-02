import Data.List (group)

main :: IO ()
main = do
  s <- getLine
  k <- readLn :: IO Int
  print $ solve s k

solve s k
  | length (group s) == 1 = (length s * k) `div` 2 -- 全体数を計算してからじゃないとズレる（重なる区間があるため）
  | otherwise = go s k
  where
    go s k
      | head s /= last s = sum (map (`div` 2) gg) * k 
      | otherwise = mid * k + (left `div` 2) + (right `div` 2) + ((left + right) `div` 2) * (k - 1)
      where
        gg = map length $ group s
        mid = sum $ map (`div` 2) (init $ tail gg)
        left = head gg
        right = last gg