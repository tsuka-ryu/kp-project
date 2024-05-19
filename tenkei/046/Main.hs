import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Map.Strict qualified as Map

main :: IO ()
main = do
  _ <- readLn :: IO Int
  xs <- replicateM 3 getInts
  let [as, bs, cs] = map Map.toList $ map countElements $ map (map (`mod` 46)) xs
  -- print [as, bs, cs]
  print $ sum [i * j * k | (a, i) <- as, (b, j) <- bs, (c, k) <- cs, (a + b + c) `mod` 46 == 0]

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

countElements :: (Ord a) => [a] -> Map.Map a Int
countElements xs = Map.fromListWith (+) [(x, 1) | x <- xs]