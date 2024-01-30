main :: IO ()
main = do
  print "template"

-- indexってなんやねん
-- ghci> import Data.Ix
-- ghci> index (0,3) 0
-- 0
-- ghci> index (0,3) 1
-- 1
-- ghci> index (0,3) 2
-- 2
-- ghci> index (0,3) 3
-- 3
-- ghci> index (0,3) 4

-- *** Exception: Ix{Integer}.index: Index (4) out of range ((0,3))

-- ghci> index (1,4) 0

-- *** Exception: Ix{Integer}.index: Index (0) out of range ((1,4))

-- ghci> index (1,4) 1
-- 0
-- ghci> index (1,4) 2
-- 1
-- ghci> index (1,4) 3
-- 2
-- ghci> index (1,4) 4
-- 3
-- ghci> index (1,4) 5

-- *** Exception: Ix{Integer}.index: Index (5) out of range ((1,4))

-- inRange (1, 4) 0
-- False

-- 二次元
-- ghci> import Data.Ix
-- ghci> index ((0,0),(2,3)) (0,1)
-- 1
-- ghci> index ((0,0),(2,3)) (0,0)
-- 0
-- ghci> index ((0,0),(2,3)) (1,0)
-- 4
-- ghci> index ((0,0),(2,3)) (2,0)
-- 8
-- ghci> rangeSize ((0,0),(2,3))
-- 12
-- ghci> inRange ((0,0),(2,3)) (2,4)
-- False