module Day1 where

import Paths_AOC2015
import Data.List (elemIndex, scanl')

day1 :: IO ()
day1 = do
  input <- (getDataDir >>= readFile . (++ "/input/input1.txt"))
  let a = scanl' (\acc -> \case '(' -> acc + 1; ')' -> acc - 1; _ -> acc) 0 input
  print $ last a
  print $ elemIndex (-1) a
