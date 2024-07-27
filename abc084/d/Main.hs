import Control.Monad (forM_, replicateM_, when)
import Data.Array.IArray (elems, listArray, (!))
import Data.Array.ST
  ( MArray (newArray),
    readArray,
    runSTUArray,
    writeArray,
  )
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (scanl', unfoldr)

upperBound :: Int
upperBound = 10 ^ (5 :: Int)

is2017like isPrime x
  | even x = False
  | otherwise = isPrime ! x && isPrime ! ((x + 1) `div` 2)

main :: IO ()
main = do
  [q] <- getInts
  let isPrime = eratosthenes upperBound
      xs = map (\i -> if is2017like isPrime i then 1 :: Int else 0) [1 .. upperBound]
      ys = listArray @UArray (1, upperBound) $ scanl' (+) (0 :: Int) xs
  -- print $ take 100 $ elems ys
  -- print "template"

  replicateM_ q $ do
    [l, r] <- getInts
    print $ ys ! (r + 1) - ys ! l

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- エラトステネスの篩
eratosthenes :: Int -> UArray Int Bool
eratosthenes n = runSTUArray $ do
  ps <- newArray (0, n) True
  mapM_ (\i -> writeArray ps i False) [0, 1]

  forM_ [2 .. n] $ \p -> do
    isPrime <- readArray ps p
    when isPrime $ do
      mapM_ (\i -> writeArray ps i False) [(p * 2), (p * 3) .. n]

  return ps
