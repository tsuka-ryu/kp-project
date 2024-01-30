import Control.Monad
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.List qualified as L
import Data.List.Extra (chunksOf)

-- Data.UArrayには関数がないので、型クラスIArrayの関数を使用する
-- Data.Arrayには関数があるが、IArrayの関数名と衝突するので、IArrayの関数を使う

main :: IO ()
main = do
  [h, w] <- ints
  mat <- getMatInt h w
  -- 配列の添字範囲
  print $ bounds mat
  -- 配列中の値の一覧（@listArray@への引数を表示）
  print $ elems mat
  -- 配列中の（添字、値）の一覧
  print $ assocs mat
  -- bounds およびassocsの組み合わせ
  print mat

ints :: IO [Int]
ints = L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 :: IO (Int, Int)
ints2 = do
  [x1, x2] <- ints
  return (x1, x2)

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
getMatInt h w = listArray ((0, 0), (h - 1, w - 1)) . concat <$> replicateM h ints

-- IArrayの主なAPIは!演算子とaccumArray
-- 多次元への1点アクセスと、畳み込み
