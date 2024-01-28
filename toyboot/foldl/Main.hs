import Debug.Trace
import GHC.List (foldl')

main :: IO ()
main = do
  print $ sum' [1, 2, 3]

sum' :: [Int] -> Int
sum' xs = foldl' step s0 xs
  where
    s0 = 0
    step acc x = acc + x
      where
        !_ = traceShow ("acc:" ++ show acc) ()
        !_ = traceShow ("x:" ++ show x) ()