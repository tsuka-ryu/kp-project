import Control.Monad (replicateM, forM_)

main :: IO ()
main = do
  [h, _] <- map read . words <$> getLine :: IO [Int]
  xs <- replicateM h (getLine :: IO String)
  let ys = concatMap (\(x, y) -> [x, y]) $ zip xs xs
  forM_ ys $ \y -> do
    putStrLn y
