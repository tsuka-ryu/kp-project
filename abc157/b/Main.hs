import Control.Monad (replicateM)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List qualified as L

main :: IO ()
main = do
  bingo <- getMat 3 3
  [n] <- ints
  bs <- replicateM n (readLn :: IO Int)
  let ls = map (`elem` bs) $ elems bingo
  let xs = listArray @UArray ((0, 0), (2, 2)) ls
  let ys = getBingoIndex bingo
  -- let res = map (\ss -> all (==True) $ map (\s -> xs ! s) ss) ys
  -- let res = any (== True) $ map (all (xs !)) ys
  -- クイックフィックスに従ったらめちゃくちゃ短くなった
  let res = any (all (xs !)) ys
  putStrLn $ if res then "Yes" else "No"

getMat :: Int -> Int -> IO (UArray (Int, Int) Int)
getMat h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getBingoIndex :: UArray (Int, Int) Int -> [[(Int, Int)]]
getBingoIndex bingo = allLines
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds bingo
    rows = [[(row, col) | col <- [minCol .. maxCol]] | row <- [minRow .. maxRow]]
    cols = [[(row, col) | row <- [minRow .. maxRow]] | col <- [minCol .. maxCol]]
    diagonals = [[(i, i) | i <- [minRow .. maxRow]], [(i, maxCol - i + minCol) | i <- [minRow .. maxRow]]]
    allLines = rows ++ cols ++ diagonals