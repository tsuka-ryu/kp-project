import Data.Char (isAsciiLower, isLower, toLower)
import Data.List qualified as L
import Debug.Trace (traceShow)

main :: IO ()
main = do
  cs <- getLine
  putStrLn $ solve cs

split :: String -> [String]
split xs = go xs
  where
    go (c : cs) | (xs, y : ys) <- span isLower cs = (c : xs ++ [y]) : go ys
      where
        !_ = traceShow c ()
        !_ = traceShow cs ()
    go [] = []

solve :: String -> String
solve = concat . L.sortOn (map toLower) . split

-- splitの処理の流れ
-- 入力:FisHDoGCaTAAAaAAbCAC
-- F:isHDoGCaTAAAaAAbCAC
-- ("is",HDoGCaTAAAaAAbCAC) <- span isLower cs
-- (F : "is" : "D") : go DoGCaTAAAaAAbCA

-- chatGPT修正版
-- split :: String -> [String]
-- split xs = go xs
--   where
--     go (c : cs) =
--       let (xs, remain) = span isLower cs in
--       case remain of
--         (y : ys) -> (c : xs ++ [y]) : go ys
--         [] -> [c : xs]
--     go [] = []

camelSplit :: String -> [String]
camelSplit [] = []
camelSplit (s : ss) = ((s : lower) ++ [s']) : camelSplit ss'
  where
    (lower, remain) = span isAsciiLower ss
    (s', ss') = (\(x : xs) -> (x, xs)) remain