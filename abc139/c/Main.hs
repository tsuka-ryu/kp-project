import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  _ <- readLn :: IO Int
  hs <- getInts
  print $ splitDescending hs 

-- chatGTP先生に書いてもらいました
splitDescending :: Ord a => [a] -> [[a]]
splitDescending [] = []
splitDescending (x:xs) = go [x] xs
  where
    go current [] = [current]
    go current (y:ys)
      | y <= last current = go (current ++ [y]) ys
      | otherwise = current : splitDescending (y:ys)

-- 10 4 8 7 3
-- [[10,4],[8,7,3]]になる

-- loop1
-- go [10] [4,8,7,3]
-- go cuurnt 4:[8,7,3]
-- 4 <= 10 = go (current ++ [4]) [8,7,3]
-- go [10,4] 8:[8,7,3]
-- otherwise = [10,4] : splitDescending (8:[8,7,3])

-- loop2
-- splitDescending (8:[7,5]) = go [8] [7,3]
-- go [8] 7:[3]
-- 7 <= 8 = go (current ++ [7]) [3]
-- go [8,7] 3
-- 3 <= 7 = go (current ++ [7]) []
-- go [8,7,3] [] = [[8,7,3]]