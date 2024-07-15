import Control.Monad (replicateM, replicateM_)
import Data.Array.IArray (listArray, (!))
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n] <- getInts
  cp <- replicateM n getInts
  [q] <- getInts
  let first = map (\[_, p] -> p) $ map (\[c, p] -> if c == 1 then [c, p] else [c, 0]) cp
  -- print first
  let f = listArray @UArray (1, n + 1) $ scanl (+) 0 first

  let second = map (\[_, p] -> p) $ map (\[c, p] -> if c == 2 then [c, p] else [c, 0]) cp
  -- print second
  let s = listArray @UArray (1, n + 1) $ scanl (+) 0 second
  -- print f
  -- print s

  replicateM_ q $ do
    [l, r] <- getInts
    let x1 = f ! (r + 1) - f ! l
    let x2 = s ! (r + 1) - s ! l
    putStrLn . unwords . map show $ [x1, x2]
