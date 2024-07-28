import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Map.Strict qualified as Map

main :: IO ()
main = do
  [_] <- getInts
  an <- getInts
  let xs = map (\a -> (a, 0)) $ scanl (+) (0 :: Int) an
  let ys = Map.toList $ Map.fromListWith (+) [(key, 1) | (key, _) <- xs]
  let ans = sum . map (\n -> (n * (n - 1)) `div` 2) $ map snd ys
  print ans

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine