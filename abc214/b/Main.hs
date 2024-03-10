import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

main :: IO ()
main = do
  [s, t] <- getInts
  let res = sum [1 :: Int | a <- [0 .. 100], b <- [0 .. 100], c <- [0 .. 100], a + b + c <= s, a * b * c <= t]
  print res

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine