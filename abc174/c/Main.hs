import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust)
import Data.List (elemIndex)

main :: IO ()
main = do
  k <- getInt

  let as = take k $ iterate (\a -> ((a * 10) + 7) `mod` k) (7 `mod` k)

  print $ case elemIndex 0 as of
    Just x -> x + 1
    Nothing -> (- 1)

getInt :: IO Int
getInt = fst . fromJust . BS.readInt <$> BS.getLine
