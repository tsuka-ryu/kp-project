import Data.Char (isDigit)

main :: IO ()
main = do
  s <- getLine
  putStrLn . handleError . solve $ s

solve :: String -> Maybe Int
solve str = if all isDigit str then Just $ read str * 2 else Nothing

handleError :: Maybe Int -> String
handleError Nothing = "error"
handleError (Just n) = show n