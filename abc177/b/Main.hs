main :: IO ()
main = do
  s <- getLine
  t <- getLine
  let xs = [x :: String | i <- [0 .. 1000], let x = drop i $ take (i + length t) s, length x == length t]
  let res = map (\x -> checkDiff x t) xs
  print $ minimum res

checkDiff x t = sum $ zipWith (\cx ct -> (if cx == ct then 0 else 1)) x t