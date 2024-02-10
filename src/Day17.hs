module Day17 where

import Control.Applicative (Alternative (empty))
import Data.List (sortBy, tails, uncons, sort, group)
import Data.Maybe (mapMaybe)

fill :: Int -> [Int] -> [[Int]]
fill n xs = go n xs'
  where
    xs' = sortBy (flip compare) xs
    go n _ | n < 0 = empty
    go n _ | n == 0 = pure []
    go n [] = empty
    go n xs = do
      (y, ys) <- mapMaybe uncons $ tails xs
      (y :) <$> go (n - y) ys

day17 :: IO ()
day17 = do
  input <- map (read @Int) . lines <$> readFile "input/input17.txt"
  let a = fill 150 input
  print $ length a
  print $ length $ head $ group $ sort $ map length a
