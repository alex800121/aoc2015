module Day10 where

import Paths_AOC2015
import Data.List (group)

input = "3113322113"

step :: String -> String
step = concatMap ((++) <$> show . length <*> (: []) . head) . group

day10 :: IO ()
day10 = do
  -- input <- (getDataDir >>= readFile . (++ "/input/input10.txt"))
  print $ length $ (!! 40) $ iterate step input
  print $ length $ (!! 50) $ iterate step input
