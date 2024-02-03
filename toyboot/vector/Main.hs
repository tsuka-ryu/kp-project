import Data.Ord (Down (Down), comparing)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU

-- sortを高速なsortBy compareに書き換える規則
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

main :: IO ()
main = do
  let xs = VU.fromList [1 :: Int, 3, 2, 4]
  print $ VU.modify VAI.sort xs
  print $ VU.modify (VAI.sortBy (comparing Down)) xs
  -- generate関数で書くこともできる
  print $ [sum [5 * na + 7 * nb | nb <- [0 .. 14]] | na <- [0 .. 20]]
  print $ VU.generate 21 $ \na -> VU.sum . VU.generate 15 $ \nb -> 5 * na + 7 * nb
  -- sotは遅い！
  print "template"

-- solve:: Int -> [Int] -> Int
-- solve k xs = inner xs
--   where
--     inner [] = 0
-- inner (x:xs) = {- .. -}

-- solveVU :: Int -> VU.Vector Int -> Int
-- solveVU k xs = inner xs
--   where
--     inner xs = case VU.uncons xs of -- (x : xs) のようなパターンマッチを uncons に置き換え
--       Nothing -> 0
--       Just (x, xs') -> x

-- vectorのソート関数はvector-algorithmsパッケージに分かれている