module Day3 where

import Data.Bifunctor (Bifunctor (..))
import Data.List (scanl', nub, transpose)
import MyLib
import Data.List.Split (divvy)

initPos = (0, 0)

walk = scanl' f initPos
  where
    f acc '>' = first (+ 1) acc
    f acc '<' = first (subtract 1) acc
    f acc '^' = second (subtract 1) acc
    f acc 'v' = second (+ 1) acc
    f acc _ = acc

day3 :: IO ()
day3 = do
  input <- readFile "input/input3.txt"
  print $ length $ nub $ walk input
  print $ length $ nub $ concatMap walk $ transpose $ divvy 2 2 input
