import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Vector.Unboxed qualified as VU

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n] <- getInts
  as <- getInts
  let xs = VU.scanl' (+) 0 $ VU.fromList as
  -- print xs

  forM_ [1 .. n] $ \i -> do
    -- let ys = [(y + i, y) | y <- [0 .. n], y + i <= n]
    -- print ys
    let zs = maximum [xs VU.! (y + i) - xs VU.! y | y <- [0 .. n], y + i <= n]
    print zs
