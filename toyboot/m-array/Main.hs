{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

import Control.Monad -- `forM_` など
import Control.Monad.ST (ST, runST) -- `ST` モナド
-- `IOArray`, `IOUArray` (`MArray` のインスタンス)
-- 可変配列の API (型クラス `MArray`)
-- `STArray`, `STUArray` (`MArray` のインスタンス)
import Data.Array.IArray
import Data.Array.IO
import Data.Array.ST
import Data.Array.Unboxed (UArray)

main :: IO ()
main = do
  let csum1 = mutCSum1 3 [1 .. 3]
  print $ elems csum1
  let csum2 = mutCSum2 3 [1.0 :: Double .. 3.0]
  print $ elems csum2
  let sum1 = mutSum1 3 [1.0 :: Double, 2.0, 3.0]
  print sum1

mutCSum1 :: Int -> [Int] -> UArray Int Int
mutCSum1 n xs = runSTUArray $ do
  arr <- newArray (0, n) 0

  forM_ (zip [0 .. n - 1] xs) $ \(!i, !dx) -> do
    acc <- readArray arr i
    writeArray arr (i + 1) $! acc + dx
  return arr

mutCSum2 :: (Num e, forall s. MArray (STUArray s) e (ST s)) => Int -> [e] -> UArray Int e
mutCSum2 n xs = runSTUArray $ do
  arr <- newArray (0, n) 0

  forM_ (zip [0 .. n - 1] xs) $ \(!i, !dx) -> do
    x <- readArray arr i
    writeArray arr (i + 1) $! x + dx
  return arr

-- 何もしない関数に通して型を付ける
asSTU :: ST s (STUArray s i e) -> ST s (STUArray s i e)
asSTU = id

-- | 累積和を計算した後に、結局和だけを返す
mutSum1 :: forall e. (Num e, forall s. MArray (STUArray s) e (ST s)) => Int -> [e] -> e
mutSum1 n xs = runST $ do
  arr <- asSTU $ newArray (0, n) 0

  forM_ (zip [0 .. n - 1] xs) $ \(!i, !dx) -> do
    x <- readArray arr i
    writeArray arr (i + 1) $! x + dx

  readArray arr n

-- なんもわからんのでGPT先生
-- このHaskellのコードは、`ST`モナドを使用して不変な配列（immutable array）を変更可能な配列（mutable array）に変換し、与えられたリストの累積和を計算するものです。以下にコードの主要な部分を解説します。

-- ```haskell
-- mutSum1 :: forall e. (Num e, forall s. MArray (STUArray s) e (ST s)) => Int -> [e] -> e
-- mutSum1 n xs = runST $ do
--   arr <- asSTU $ newArray (0, n) 0
-- ```

-- 1. `mutSum1`関数の型宣言は、型変数 `e` が `Num` クラスのインスタンスであり、さらに任意の `s` について `STUArray s` 型の mutable array に `MArray` クラスが適用されることを示しています。`STUArray s` は `ST` モナド内で使用される mutable array の型です。

-- 2. `runST`は、ST モナド内での計算を不変性を保ちながら実行するための関数です。この関数は、`ST` モナドから通常の Haskell 値に変換する役割を果たします。

-- 3. `asSTU`関数は、immutable array を mutable array に変換するためのヘルパー関数です。

-- 4. `newArray (0, n) 0` は、長さ `n + 1` の全ての要素が 0 で初期化された mutable array を生成します。

-- ```haskell
--   forM_ (zip [0 .. n - 1] xs) $ \(!i, !dx) -> do
--     x <- readArray arr i
--     writeArray arr (i + 1) $! x + dx
-- ```

-- 5. `forM_`関数は、与えられたリストと対応する要素に対してモナド内でアクションを実行するためのループです。ここでは、`zip [0 .. n - 1] xs` を使って、リスト `xs` の各要素とそのインデックスのペアを生成しています。

-- 6. ループ内では、`readArray arr i` で `arr` のインデックス `i` の値を読み取り、`writeArray arr (i + 1) $! x + dx` で `arr` のインデックス `i + 1` に `x + dx` の値を書き込んでいます。これにより、累積和が計算されていきます。

-- ```haskell
--   readArray arr n
-- ```

-- 7. 最後に、計算された累積和を `arr` の最後の要素（インデックス `n`）から読み取っています。

-- このコードは、`ST` モナドを使用して不変性を維持しつつ mutable array を操作し、リストの累積和を高効率に計算する方法を示しています。