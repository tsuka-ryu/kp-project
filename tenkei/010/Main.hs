import Control.Monad (replicateM, replicateM_)
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr, scanl')

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getTuple :: IO (Int, Int)
getTuple = do
  [a, b] <- getInts
  return (a, b)

main :: IO ()
main = do
  [n] <- getInts
  cp <- replicateM n getTuple
  [q] <- getInts
  let cp' = map (\(c, p) -> if c == 1 then (p, 0) else (0, p)) cp
  let f = listArray @UArray (1, n + 1) $ scanl' (+) 0 $ map fst cp'
  let s = listArray @UArray (1, n + 1) $ scanl' (+) 0 $ map snd cp'

  replicateM_ q $ do
    [l, r] <- getInts
    let x1 = f ! (r + 1) - f ! l
    let x2 = s ! (r + 1) - s ! l
    putStrLn . unwords . map show $ [x1, x2]
