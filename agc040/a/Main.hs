import Data.List (foldl', group)

main :: IO ()
main = do
  s <- getLine
  print $ solve s

solve xs = fst . foldl' step (0, 0) . group $ xs
  where
    step (!acc, !h) s@('<' : _) = (acc + sum [h + 1 .. h + len], h + len)
      where
        !len = length s
    step (!acc, !h) s@('>' : _)
      | h >= len = (acc + sum [0 .. len - 1], 0)
      | otherwise = (acc + sum [0 .. len - 1] + len - h, 0)
      where
        !len = length s

-- 解説読んだけど実装できないので写経
-- '<'で積み上げるグループごとに累積させたいので、'>'の場合は0にリセットできるらしい
-- というのも、連続した値という条件はないから、>の一番最後は0にするのがよい
-- 前の状態を引き継ぐわけではなく、「累積」だからリセットできるのか

-- step (!acc, !h) s@('>' : _)
--   | h >= len = (acc + sum [0 .. len - 1],  h - len)
-- こうすると累積結果がおかしくなるらしい