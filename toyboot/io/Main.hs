{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.Array qualified as A
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt, isSpace)
import Data.List qualified as L

main :: IO ()
main = do
  print "template"
  -- vector, arrayを空白区切りで出力するには一度リストに変換する
  putStrLn $ unwords . map show . elems $ A.listArray (1, 3) [1 .. 3]
  -- unlinesを適用すると、改行区切りの文字列になる
  putStr $ unlines . map show $ [1 :: Int, 2, 3]
  -- forM_とか使っても良い
  forM_ [1 :: Int, 2, 3] print

yn :: Bool -> String
yn = bool "No" "Yes"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

-- 入力はByteStringを使った方が高速らしい？
-- 1. 整数列
ints :: IO [Int]
ints = map read . words <$> getLine

ints' :: IO [Int]
ints' = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- 2. 2個の整数
-- lambdaCaseの指定が必要
-- \case in place of \arg -> case arg of
ints2 :: IO (Int, Int)
ints2 =
  ints >>= \case
    [x1, x2] -> return (x1, x2)
    _ -> error "ints2: wrong number of integers"

-- 3. 巨大整数
-- Intに収まらない場合は、各桁を[Int]として保存する
digitsL :: IO [Int]
digitsL = L.unfoldr (fmap (first digitToInt) . BS.uncons) <$> BS.getLine

-- 4. グリッド
-- 型エラーが直せない
-- getGrid :: Int -> Int -> IO (UArray (Int, Int) Char)
-- getGrid h w = listArray ((0, 0) (h - 1, w - 1)) . concatMap BS.unpack <$> replicateM h BS.getLine

-- 5. 行列
getMat :: Int -> Int -> IO (UArray (Int, Int) Int)
getMat h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints