import Data.List (mapAccumL)

main :: IO ()
main = do
  print $ scanl1 (+) [1, 2, 3]
  print $ invCSum $ scanl1 (+) [1, 2, 3]
  print $ mapAccumL (\x lastX -> (x + lastX, lastX)) 0 [1, 2, 3]

invCSum :: [Int] -> (Int, [Int])
invCSum xs = mapAccumL step s0 xs
  where
    s0 = 0 :: Int
    step lastX x = (x, x - lastX)