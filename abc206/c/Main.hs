import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Map.Strict qualified as Map

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

countElements :: (Ord a) => [a] -> Map.Map a Int
countElements xs = Map.fromListWith (+) [(x, 1) | x <- xs]

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- getInts
  let xs = countElements as
  let ys = map (\(_, q) -> (q * (q - 1)) `div` 2) $ Map.toList xs
  -- print xs
  -- print ys
  print $ (n * (n - 1)) `div` 2 - sum ys
