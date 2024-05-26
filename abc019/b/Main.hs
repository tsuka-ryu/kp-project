import Data.List as L

main :: IO ()
main = do
  s <- getLine
  let xs = concatMap (\(p,q) -> p ++ show q) $ map (\x -> (take 1 x, length x)) $ L.group s
  putStrLn xs 
