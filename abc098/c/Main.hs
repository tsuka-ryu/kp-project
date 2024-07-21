import Data.Vector.Unboxed qualified as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- getLine
  let ws = VU.scanl' (\acc c -> if c == 'W' then acc + 1 else acc) (0 :: Int) $ VU.fromList s
  let es = VU.scanl' (\acc c -> if c == 'E' then acc + 1 else acc) (0 :: Int) $ VU.fromList s
  -- let res = [(w, e) | i <- [1 .. n], let w = ws VU.! (i - 1) - ws VU.! 0, let e = es VU.! n - es VU.! i]
  let res = minimum [w + e | i <- [1 .. n], let w = ws VU.! (i - 1) - ws VU.! 0, let e = es VU.! n - es VU.! i]
  print res
