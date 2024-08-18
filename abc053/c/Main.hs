import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust)

main :: IO ()
main = do
  x <- getInt
  let (a, b) = x `divMod` 11
  let ans = a * 2 + f b
  print ans

f b
  | b == 0 = 0
  | b <= 6 = 1 -- 6以下なら1回追加
  | otherwise = 2 -- 6より大きければ2回追加で転がす

getInt :: IO Int
getInt = fst . fromJust . BS.readInt <$> BS.getLine
