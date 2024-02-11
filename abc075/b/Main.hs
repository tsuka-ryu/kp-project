import Control.Monad (forM_, replicateM)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Char (intToDigit)
import Data.List.Extra (chunksOf)

main :: IO ()
main = do
  [h, w] <- map read . words <$> getLine :: IO [Int]
  mat <- getMatChar h w
  let idx = [(x, y) | x <- [0 .. h - 1], y <- [0 .. w - 1]]
  let tmp = map (\(x, y) -> solve (x, y, mat)) idx
  forM_ (chunksOf w tmp) $ \row -> do
    putStrLn row

getMatChar :: Int -> Int -> IO (UArray (Int, Int) Char)
getMatChar h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h getLine

solve :: (Int, Int, UArray (Int, Int) Char) -> Char
solve (x, y, mat) =
  if isSharp
    then '#'
    else intToDigit countSharpNeighbors
  where
    isSharp = mat ! (x, y) == '#'
    -- 自分の周囲の座標を計算
    neighborIndices = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]
    -- 座標が範囲内か確認
    inBounds nx ny = let ((minX, minY), (maxX, maxY)) = bounds mat in nx >= minX && nx <= maxX && ny >= minY && ny <= maxY
    -- 結果を計算
    countSharpNeighbors = length $ filter (\(nx, ny) -> inBounds nx ny && mat ! (nx, ny) == '#') neighborIndices
