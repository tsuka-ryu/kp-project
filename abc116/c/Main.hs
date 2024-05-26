import Control.Monad
import Data.List

main :: IO ()
main = do
  _ <- getLine
  as <- map (read :: String -> Int) . words <$> getLine
  let h = maximum as
  let xs = filter or . group $ map (>= 1) as -- 全部がFalseのグループをfilterしてる
  let ys = filter or . group $ map (>= 2) as
  let zs = filter or . group $ map (>= 3) as
  print xs
  print ys
  print zs
  print $ length $ join $ map (\i -> filter or . group $ map (>= i) as) [1 .. h]
