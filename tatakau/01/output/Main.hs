import Data.List (intercalate, intersperse)

main :: IO ()
main = do
  -- 空白で連結
  let wordsList = ["Hello", "world", "this", "is", "Haskell"]
  let result = unwords wordsList
  putStrLn result
  -- 改行で連結
  let linesList = ["Hello", "world", "this", "is", "Haskell"]
  let result = unlines linesList
  putStrLn result
  -- カンマ区切りで連結
  let listOfLists = ["Hello", "world", "this", "is", "Haskell"]
  let result = intercalate ", " listOfLists
  putStrLn result
  -- ハイフンで分割
  let list = "Haskell"
  let result = intersperse '-' list
  putStrLn result
  -- ByteString.Builderというのもあるらしい
