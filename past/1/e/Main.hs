{-# LANGUAGE TupleSections #-}

import Control.Monad (forM_, replicateM)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', unfoldr)
import Debug.Trace (traceShow)

type Graph = IM.IntMap IS.IntSet

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

follow :: Int -> Int -> Graph -> Graph
follow a b = IM.adjust (IS.insert b) a

fb :: Int -> Graph -> Graph
-- flipは引数の順序を逆にできる
fb a g = foldl' (flip (follow a)) g followers
  where
    -- IM.keys gはグラフの全ノードのリストを返します。
    -- filter (\v -> IS.member a (g IM.! v))は、各ノードvに対して、aがそのノードvの隣接ノードの集合に含まれているかを確認します。含まれている場合、そのノードvをfollowersに追加します。
    -- したがって、followersはノードaをフォローしているノードのリストです。
    followers = filter (\v -> IS.member a (g IM.! v)) (IM.keys g)
    !_ = traceShow followers ()

-- ユーザー１がフォローしてるユーザー2,5で、ユーザー２は3を、ユーザー5は6をフォローしてる
-- ユーザー１は3,6をフォローする
ff :: Int -> Graph -> Graph
ff a g0 = foldl' f g0 following
  where
    following = IS.toList (g0 IM.! a)

    f :: Graph -> Int -> Graph
    f g v = IM.adjust (`IS.union` vs) a g -- vの隣接ノードを取得して、ユニオンにしたものをaのノードに入れ込む
      where
        -- aを削除した集合
        vs = IS.delete a (g IM.! v)

run :: [[Int]] -> Graph -> Graph
run qs g0 = foldl' f g0 qs
  where
    f g [1, a, b] = follow a b g
    f g [2, a] = fb a g
    f g [3, a] = ff a g
    f _ _ = error "Invalid query"

main :: IO ()
main = do
  [n, q] <- getInts
  qs <- replicateM q getInts

  -- 初期の配列（Setしてる） 
  -- [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),(5,fromList []),(6,fromList [])]
  -- print $ map (,IS.empty) [1 .. n]
  -- fromList [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),(5,fromList []),(6,fromList [])]
  -- print $ IM.fromList $ map (,IS.empty) [1 .. n]

  let g0 = IM.fromList $ map (,IS.empty) [1 .. n]
      g = run qs g0
  -- qsの実行後の結果
  -- fromList [(1,fromList [2,3,5,6]),(2,fromList [3]),(3,fromList [4]),(4,fromList []),(5,fromList [6]),(6,fromList [1,5])]
  -- print g

  forM_ g $ \vs -> do
    putStrLn $ map (\i -> if i `IS.member` vs then 'Y' else 'N') [1 .. n]