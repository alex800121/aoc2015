module Day2 where

import Data.List (sort)
import Data.List.Split (splitOn)
import MyLib (pick)

day2 :: IO ()
day2 = do
  input <- map (map (read @Int) . splitOn "x") . lines <$> readFile "input/input2.txt"
  print $ sum $ map (((+) <$> minimum <*> (* 2) . sum) . map product . pick 2) input
  print $ sum $ map (((+) <$> product <*> (* 2) . sum . take 2) . sort) input
