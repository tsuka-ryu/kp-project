import Data.List (sort)

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ minimum $ sort [f a b, f b a]

f :: Int -> Int -> String
f x y = concatMap show $ replicate x y