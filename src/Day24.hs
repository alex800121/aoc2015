module Day24 where

import Control.Monad (guard)
import Data.Array
import Data.List (delete, sort, sortBy, uncons, insert, minimumBy)
import Data.Maybe (maybeToList)
import MyLib (pickAnySplit)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (on)


sumV :: Int -> Int -> [Int] -> [[Int]]
sumV s n _ | s < 0 || n < 0 = []
sumV 0 _ _ = [[]]
sumV _ _ [] = []
sumV m n (x : xs)
  | x <= m = map (x :) (sumV (m - x) (n - 1) xs) <> sumV m n xs
  | otherwise = sumV m n xs


day24 :: IO ()
day24 = do
  input <- sort . map (read @Int) . lines <$> readFile "input/input24.txt"
  let s = sum input `div` 3
      l = length input `div` 3
      s' = sum input `div` 4
      l' = length input `div` 4
  print $ product $ minimumBy (\x y -> on compare length x y <> on compare product x y) $ sumV s l input
  print $ product $ minimumBy (\x y -> on compare length x y <> on compare product x y) $ sumV s' l' input
