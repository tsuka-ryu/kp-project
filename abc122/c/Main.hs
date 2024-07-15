{-# LANGUAGE TypeApplications #-}

import Control.Monad (replicateM_)
import Data.Array.IArray (listArray, (!))
import Data.Array.Unboxed (UArray)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n, q] <- getInts
  s <- BS.getLine

  let t =
        listArray @UArray (1, n) $
          scanl (\acc xx -> if xx == "AC" then acc + 1 else acc) (0 :: Int) $
            BS.zipWith (\a b -> [a, b]) s (BS.tail s)
  -- let bs = BS.zipWith (\a b -> [a, b]) s (BS.tail s)
  -- print bs
  -- ["AC","CA","AC","CT","TA","AC","CG"]
  -- print $ scanl (\acc xx -> if xx == "AC" then acc + 1 else acc) (0 :: Int) $
  --           BS.zipWith (\a b -> [a, b]) s (BS.tail s)
  -- [0,1,1,2,2,2,3,3]
  -- print t
  -- array (1,8) [(1,0),(2,1),(3,1),(4,2),(5,2),(6,2),(7,3),(8,3)]k

  replicateM_ q $ do
    [l, r] <- getInts
    print $ t ! r - t ! l