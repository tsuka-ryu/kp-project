import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (group, unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

rle :: (Eq a) => [a] -> [(a, Int)]
rle = map (\x -> (head x, length x)) . group

main :: IO ()
main = do
  _ <- readLn :: IO Int
  hs <- getInts
  let xs = sum . map (\(_, m) -> m `div` 2) $ rle hs
  print xs

-- 連続してる数に対する規則を探す
-- 1 -> 0
-- 2 -> 1
-- 3 -> 1
-- 4 -> 2
-- 5 -> 2
-- 6 -> 3
-- 7 -> 3
-- 8 -> 4
-- 9 -> 4