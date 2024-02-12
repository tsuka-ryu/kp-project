import Control.Monad
import Data.Array
import Debug.Trace (traceShow)

main = do
  [h, w] <- map read . words <$> getLine
  ss <- replicateM h getLine
  let ans = abc075b h w ss
  print $ tcnts h w ss
  print $ zip [1 ..] ss
  print $ [(j, '#') | (j, '#') <- zip [1 ..] "....."]
  print $ [(j, '#') | (j, '#') <- zip [1 ..] ".#.#."]
  print $ [(j, '#') | (j, '#') <- zip [1 ..] "....."]
  -- #の位置が取れる
  --   []
  -- [(2,'#'),(4,'#')]
  -- []
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

tcnts h w ss =
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