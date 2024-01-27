import Data.Vector.Unboxed qualified as U

main :: IO ()
main = do
  let x = digits10'U 12345
  print $ length $ filter (== True) $ [True, False, True]
  print x

digits10'U :: Int -> U.Vector Int
digits10'U = U.unfoldr f
  where
    f 0 = Nothing
    f x = Just (r, q)
      where
        (q, r) = x `divMod` 10