import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  cs <- replicateM 3 getInts
  let a1 = 0
      [b1, b2, b3] = head cs
  let a2 = cs !! 1 !! 0 - b1
      a3 = cs !! 2 !! 0 - b1
  let css = [a + b | a <- [a1, a2, a3], b <- [b1, b2, b3]]
      xs = concat cs
  putStrLn $ if and (zipWith (==) css xs) then "Yes" else "No"