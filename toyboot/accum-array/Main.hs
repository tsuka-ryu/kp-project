import Data.Array.IArray
import Data.Array.Unboxed (UArray)

main :: IO ()
main = do
  print $ accumArray @UArray (+) (0 :: Int) (0 :: Int, 3) []
  print $ accumArray @UArray (+) (0 :: Int) (0 :: Int, 3) [(0, 1)]
  print $ accumArray @UArray (+) (0 :: Int) (0 :: Int, 3) [(0, 1), (3, 2)]
  print $ accumArray @UArray (+) (0 :: Int) (0 :: Int, 3) [(0, 1), (3, 2), (0, 3)]

-- (index, value)のタプルのペア
-- array (0,3) [(0,0),(1,0),(2,0),(3,0)]
-- array (0,3) [(0,1),(1,0),(2,0),(3,0)]
-- array (0,3) [(0,1),(1,0),(2,0),(3,2)]
-- array (0,3) [(0,4),(1,0),(2,0),(3,2)]
