module Day15 where

import Paths_AOC2015
import Data.Bifunctor (Bifunctor (..))
import Data.List (transpose)

type Ingredient = ([Int], Int)

-- Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
inputParser :: String -> Ingredient
inputParser s
  | [_, _, a, _, b, _, c, _, d, _, e] <- words s =
      (map (read . init) [a, b, c, d], read e)

splitN :: (Integral a, Integral b) => a -> b -> [[a]]
splitN a b
  | a == 0 && b <= 0 = pure []
  | a == 0 && b > 0 = []
  | a > 0 && b <= 0 = []
  | a > 0 && b > 0 = do
      x <- [0 .. a]
      (x :) <$> splitN (a - x) (b - 1)

day15 :: IO ()
day15 = do
  input <- map inputParser . lines <$> (getDataDir >>= readFile . (++ "/input/input15.txt"))
  -- input <- map inputParser . lines <$> readFile "input/test15.txt"
  let l = length input
      n = splitN (100 :: Int) l
      day15a =
        map
          ( first (product . map (max 0))
              . foldr (\(x, y) (a, b) -> (zipWith (+) x a, y + b)) (repeat 0, 0)
              . zipWith (\(i, c) b -> (map (* b) i, c * b)) input
          )
          n
  -- print $ maximumBy (compare `on` fst) day15a
  print $ maximum $ map fst day15a
  print $ maximum $ map fst $ filter ((== 500) . snd) day15a
