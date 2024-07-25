module Day25 where

input = (3029, 2947)

go target t@(x, y) i
  | target == t = i
  | y == 1 = go target (1, x + 1) i'
  | otherwise = go target (x + 1, y - 1) i'
  where
    i' = (i * 252533) `mod` 33554393

day25 :: IO ()
day25 = do
  -- input <- (getDataDir >>= readFile . (++ "/input/input25.txt"))
  print $ go input (1, 1) 20151125
