import Control.Monad -- `forM_` など
import Control.Monad.ST -- `ST` モナド
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.IO -- `IOArray`, `IOUArray` (`MArray` のインスタンス)
import Data.Array.MArray -- 可変配列の API (型クラス `MArray`)
import Data.Array.ST -- `STArray`, `STUArray` (`MArray` のインスタンス)
import Data.Array.Unboxed (UArray)

mutCSum1 :: Int -> [Int] -> UArray Int Int
mutCSum1 n xs = runSTUArray $ do
  arr <- newArray (0, n) 0

  forM_ (zip [0 .. n - 1] xs) $ \(!i, !dx) -> do
    acc <- readArray arr i
    writeArray arr (i + 1) $! acc + dx
  return arr

main :: IO ()
main = do
  let csum1 = mutCSum1 3 [1 .. 3]
  print $ elems csum1