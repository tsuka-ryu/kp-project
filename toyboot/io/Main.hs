import Control.Monad
import Data.Array
import Data.Bool (bool)

main :: IO ()
main = do
  print "template"
  -- vector, arrayを空白区切りで出力するには一度リストに変換する
  putStrLn $ unwords . map show . elems $ listArray (1, 3) [1 .. 3]
  -- unlinesを適用すると、改行区切りの文字列になる
  putStr $ unlines . map show $ [1 :: Int, 2, 3]
  -- forM_とか使っても良い
  forM_ [1 :: Int, 2, 3] print

yn :: Bool -> String
yn = bool "No" "Yes"

printYn :: Bool -> IO ()
printYn = putStrLn . yn