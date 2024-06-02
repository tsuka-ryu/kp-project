import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

countSegments :: [Int] -> Int
countSegments [] = 0
countSegments [_] = 1
countSegments (x : y : xs)
  | x == y = countSegments (y : xs) -- 同じ要素をスキップ
  | x < y = 1 + countSegments (map snd (dropWhile (uncurry (<=)) (zip (y : xs) xs))) -- 上昇セグメントの終わりまでスキップ
  | otherwise = 1 + countSegments (map snd (dropWhile (uncurry (>=)) (zip (y : xs) xs))) -- 下降セグメントの終わりまでスキップ

main :: IO ()
main = do
  _ <- getLine
  a <- getInts
  print (countSegments a)

-- chatGPTに書いてもらって手直しした