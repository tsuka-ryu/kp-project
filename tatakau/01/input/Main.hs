import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

main :: IO ()
main = do
  -- 標準入力
  -- 123
  -- 4 5
  -- six
  -- a <- readLn :: IO Int
  -- [b, c] <- map read . words <$> getLine :: IO [Int]
  -- s <- getLine
  -- putStrLn $ show (a + b + c) ++ " " ++ s
  -- ByteString
  -- map read .wordsは遅い
  s <- BS.getLine
  [b, c] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print (s, b, c)
  -- 文字と数字が混在している場合
  -- hoge 1 のような入力
  [str, num] <- BS.words <$> BS.getLine
  let Just (x, _) = BS.readInt num
  print (str, num, x)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
