{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

import Control.Monad -- `forM_` など
import Control.Monad.ST (ST) -- `ST` モナド
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.IO -- `IOArray`, `IOUArray` (`MArray` のインスタンス)
import Data.Array.MArray -- 可変配列の API (型クラス `MArray`)
import Data.Array.ST -- `STArray`, `STUArray` (`MArray` のインスタンス)
import Data.Array.Unboxed (UArray)

main :: IO ()
main = do
  let csum1 = mutCSum1 3 [1 .. 3]
  print $ elems csum1
  let csum2 = mutCSum2 3 [1.0 :: Double .. 3.0]
  print $ elems csum2

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