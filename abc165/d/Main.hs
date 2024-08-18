import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [a, b, n] <- getInts
  -- let res = map (\i -> f a b i) [1 .. n]
  let g = f a b
  print $ g $ min (b - 1) n

f a b i = ((a * i) `div` b) - a * (i `div` b)