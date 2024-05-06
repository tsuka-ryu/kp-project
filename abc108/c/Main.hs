import Debug.Trace (traceShow)

countRemainders :: Int -> Int -> Integer
countRemainders n k = sum [(num !! a) * (num !! b) * (num !! c) | a <- [0 .. k - 1], let b = (k - a) `mod` k, let c = (k - a) `mod` k, (b + c) `mod` k == 0, let !_ = traceShow (a, b, c) ()]
  where
    num = map (\x -> fromIntegral $ length $ filter (\y -> y `mod` k == x) [1 .. n]) [0 .. k - 1]

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine :: IO [Int]
  print $ countRemainders n k
  print $ map (\x -> filter (\y -> y `mod` k == x) [1 .. n]) [0 .. k - 1]

-- numって何？
-- n=5, k=3
-- [1,2,3,4,5] `mod` 3 == 0 -> [3]
-- [1,2,3,4,5] `mod` 3 == 1 -> [1,4]
-- [1,2,3,4,5] `mod` 3 == 2 -> [2,5]

-- a mod k(3)=[0,1,2]
-- a=0, b=0, c=0 -> 1*1*1 = 1 
-- a=1, b=2, c=2 -> 対象外
-- a=2, b=1, c=1 -> 対象外

-- n=3, k=2
-- [1,2,3] `mod` 2 == 0 -> [2]
-- [1,2,3] `mod` 2 == 1 -> [1,3]

-- a mod k(2)=[0,1]
-- a=0, b=0, c=0 -> 1*1*1 = 1 
-- a=1, b=1, c=1 -> 2*2*2 = 8 

-- num !! hogeの部分は、組み合わせの数を数えている
-- a mod k = 0がありうるのは、n=2のときだけ
-- a mod k = 1がありうるのは、n=1,3のとき
-- 同じ理屈で、bとcも、ありうる値はnum !! hogeで計算される