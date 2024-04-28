import Data.List (nub)

main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- getLine
  let xs = [splitAt i s | i <- [1 .. n - 1]]
  -- let xs = [(take i s, drop i s) | i <- [1 .. n - 1]]
  let res = map countCommonChar xs
  print $ maximum res

countCommonChar (a :: String, b :: String) = length common
  where
    nubA = nub a
    nubB = nub b
    common = filter (`elem` nubA) nubB