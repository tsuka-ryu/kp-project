import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Vector.Unboxed qualified as VU

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n, k] <- getInts
  let p = n - k + 1
  as <- getInts
  let xs = VU.scanl' (+) 0 $ VU.fromList as
      qs = map (\x -> (x, x + k - 1)) [1 .. p]

  -- print qs

  forM_ qs $ \(lower, upper) -> do
    print $ xs VU.! upper - xs VU.! (lower - 1)
