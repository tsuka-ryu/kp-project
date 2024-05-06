import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (nub, unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n] <- getInts
  cs <- concat <$> replicateM n getInts
  print $ length $ nub cs
