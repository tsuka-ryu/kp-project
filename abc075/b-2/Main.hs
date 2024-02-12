import Control.Monad
import Data.Array

main = do
  [h, w] <- map read . words <$> getLine
  ss <- replicateM h getLine
  let ans = abc075b h w ss
  mapM_ putStrLn ans

abc075b :: Int -> Int -> [String] -> [String]
abc075b h w ss = ans
  where
    cnts =
      accumArray
        (+)
        0
        ((1, 1), (h, w))
        [ ((x, y), 1)
          | (i, s) <- zip [1 ..] ss,
            (j, '#') <- zip [1 ..] s,
            x <- [pred i | i > 1] ++ i : [succ i | i < h],
            y <- [pred j | j > 1] ++ j : [succ j | j < w]
        ]
    ans =
      [ [ if c == '#' then c else (head $ show $ cnts ! (i, j))
          | (j, c) <- zip [1 ..] s
        ]
        | (i, s) <- zip [1 ..] ss
      ]

-- 8近傍の#の個数で.を置き換える
-- この部分は、まず#のある場所を特定している
-- したのデータの場合、(2,2) (2,4）が#の位置
-- 3 5
-- .....
-- .#.#.
-- .....

-- #の位置がわかったら、その周囲の値を計算する
-- (2,2)のとき
-- x = [1,2,3]
-- y = [1,2,3]
-- xとyの非決定性計算で[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
-- (2,4）のとき
-- x = [1,2,3]
-- y = [3,4,5]
-- xとyの非決定性計算で[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]
-- この結果をたたみ込んで、答えが出る
-- [ ((x,y),1)
--       | (i,s) <- zip [1..] ss
--       , (j,'#') <- zip [1..] s
--       , x <- [pred i | i > 1] ++ i : [succ i | i < h]
--       , y <- [pred j | j > 1] ++ j : [succ j | j < w]
--       ]