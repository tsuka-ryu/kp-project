import Data.Array.IArray (listArray, (!))
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)
import Text.Printf (printf)

main :: IO ()
main = do
  [_, k] <- getInts
  px <- getInts

  let exp' = listArray @UArray (1, 1000) [culcExp i | i <- [1 :: Int .. 1000]]
  -- print $ take 10 exp
  let t = scanl (\acc p -> acc + exp' ! p) (0 :: Double) px
  let x = maximum $ zipWith (\a b -> b - a) t (drop k t)
  printf "%.12f\n" x

culcExp :: Int -> Double
culcExp i = sum [fromIntegral x / fromIntegral i | x <- [1 .. i]]

-- 期待値の計算
-- 1 * 1 / 1 = 1
-- 1 * 1/2 + 2 * 1/2 = 1.5
-- 1 * 1/4 + 2 * 1/4 + 3 * 1/4 * 4 * 1/4 = 2.5
-- 1 * 1/5 + 2 * 1/5 + 3 * 1/5 * 4 * 1/5 + 5 * 1/5 = 3

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
