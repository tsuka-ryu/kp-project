import Control.Monad (forM_, replicateM)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.List.Extra (chunksOf)

main :: IO ()
main = do
  [h, w] <- map read . words <$> getLine :: IO [Int]
  mat <- getMatChar h w
  -- 自分の周囲の座標、右隣から時計回り
  -- let around = [(1 :: Int, 0 :: Int), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]
  let idx = [(x, y) | x <- [0 .. h - 1], y <- [0 .. w - 1]]
  let tmp = map (\(x, y) -> solve (x, y, mat)) idx
  -- print mat
  -- print tmp
  forM_ (chunksOf w tmp) $ \row -> do
    -- 各行を表示する
    putStrLn row

-- listArrayはその名の通り、リストからArrayを生成する関数
getMatChar :: Int -> Int -> IO (UArray (Int, Int) Char)
getMatChar h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h getLine

solve :: (Num a1, Num b, IArray a2 Char, Ix a1, Ix b) => (a1, b, a2 (a1, b) Char) -> Char
solve (x, y, mat) = if isSharp then m else charAns
  where
    m = mat ! (x, y)
    isSharp = m == '#'
    lowerX = fst . fst $ bounds mat
    lowerY = snd . fst $ bounds mat
    upperX = fst . snd $ bounds mat
    upperY = snd . snd $ bounds mat
    right = if x + 1 <= upperX then mat ! (x + 1, y) else '.'
    rightDown = if x + 1 <= upperX && y + 1 <= upperY then mat ! (x + 1, y + 1) else '.'
    down = if y + 1 <= upperY then mat ! (x + 0, y + 1) else '.'
    leftDown = if x - 1 >= lowerX && y + 1 <= upperY then mat ! (x - 1, y + 1) else '.'
    left = if x - 1 >= lowerX then mat ! (x - 1, y) else '.'
    leftUp = if x - 1 >= lowerX && y - 1 >= lowerY then mat ! (x - 1, y - 1) else '.'
    up = if y - 1 >= lowerY then mat ! (x, y - 1) else '.'
    rightUp = if x + 1 <= upperX && y - 1 >= lowerY then mat ! (x + 1, y - 1) else '.'
    ans = length $ filter (== '#') [right, rightDown, down, leftDown, left, leftUp, up, rightUp]
    charAns = toEnum (ans + fromEnum '0') -- Int -> Charにする

-- 方針
-- 周囲のx,y座標はaroundで持っている
-- arrayの値一つ一つにアクセスして、それぞれaroundで値を取得してチェック -- ixmapは一次元のArrayにしか使えない？
-- アクセス時には、範囲外の場合に例外にならないようにする