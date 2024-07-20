import Control.Monad (replicateM, replicateM_)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Vector.Unboxed qualified as VU

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
  let cp' = map (\(c, p) -> if c == 1 then (p, 0) else (0, p)) cp
  let f = VU.scanl' (+) 0 $ VU.fromList $ map fst cp'
  let s = VU.scanl' (+) 0 $ VU.fromList $ map snd cp'

  [q] <- getInts
  replicateM_ q $ do
    [l, r] <- getInts
    let x1 = f VU.! r - f VU.! (l - 1)
    let x2 = s VU.! r - s VU.! (l - 1)
    putStrLn . unwords . map show $ [x1, x2]
