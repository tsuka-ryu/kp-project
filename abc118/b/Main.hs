import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

omitFirstK :: [a] -> [a]
omitFirstK (_ : rest) = rest
omitFirstK [] = []

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine :: IO [Int]
  as <- concat <$> replicateM n (omitFirstK <$> getInts)
  let xs = [(i, 0 :: Int) | i <- [1 .. m]]
  let ys = map (\(p, _) -> (p, length $ filter (== p) as)) xs
  let ans = length $ filter (\(_, y) -> y == n) ys
  -- print [n, m]
  -- print as
  -- print xs
  -- print ys
  print ans
