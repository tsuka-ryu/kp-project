import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

main :: IO ()
main = do
  [_] <- getInts
  an <- getInts
  let left = scanl (\acc a -> gcd acc a) (0 :: Int) $ an
  let right = scanr (\acc a -> gcd acc a) (0 :: Int) $ an
  let res = zipWith (gcd) left (tail right)
  -- print left
  -- print right
  print $ maximum res

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine