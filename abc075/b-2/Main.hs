import Control.Monad
import Data.Array
import Debug.Trace (traceShow)

main = do
  [h, w] <- map read . words <$> getLine
  ss <- replicateM h getLine
  let ans = abc075b h w ss
  mapM_ putStrLn ans

abc075b :: Int -> Int -> [String] -> [String]
abc075b h w ss = ans
  where
    input =
      [ ((x, y), 1)
        | (i, s) <- zip [1 ..] ss,
          (j, '#') <- zip [1 ..] s,
          -- リスト内包表記
          -- ghci> [pred 0 | 0 > 1]
          -- []
          -- ghci> [pred 1 | 1 > 1]
          -- []
          -- ghci> [pred 2 | 2 > 1]
          -- [1]
          -- ghci> [pred 3 | 3 > 1]
          -- [2]
          -- ghci> [pred 4 | 4 > 1]
          -- [3]
          x <- [pred i | i > 1] ++ i : [succ i | i < h],
          y <- [pred j | j > 1] ++ j : [succ j | j < w]
      ]
    !_ = traceShow input ()
    -- [((1,1),1),((1,2),1),((1,3),1),
    -- ((2,1),1),((2,2),1),((2,3),1),
    -- ((3,1),1),((3,2),1),((3,3),1),
    -- ((1,3),1),((1,4),1),((1,5),1),
    -- ((2,3),1),((2,4),1),((2,5),1),
    -- ((3,3),1),((3,4),1),((3,5),1)]
    cnts =
      accumArray
        (+) -- function
        0 -- 初期値
        ((1, 1), (h, w)) -- 配列の範囲
        -- 引数
        input
    -- cntsのi,j番目(行、列）にアクセスする
    ans =
      [ [ if c == '#' then c else (head $ show $ cnts ! (i, j))
          | (j, c) <- zip [1 ..] s
        ]
        | (i, s) <- zip [1 ..] ss
      ]

-- 8近傍の#の個数で.を置き換える