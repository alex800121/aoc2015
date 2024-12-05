module Day14 where

import Data.List (foldl')
import Paths_AOC2015

type Reindeer = ((Int, Int), Int)

-- Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.
inputParser :: String -> Reindeer
inputParser s
  | [_, _, _, v, _, _, t0, _, _, _, _, _, _, t1, _] <- words s =
      ((read t0, read t1), read v)

distance :: Reindeer -> Int -> Int
distance ((t0, t1), v) t = v * tx
  where
    c = t0 + t1
    (a, b) = t `divMod` c
    tx = (a * t0) + min b t0

awardPoint :: [Int] -> [Int]
awardPoint l = map (\x -> if x == m then 1 else 0) l
  where
    m = maximum l

day14 :: IO ()
day14 = do
  input <- map inputParser . lines <$> (readFile . (++ "/input/input14.txt") =<< getDataDir)
  print $ maximum $ map (`distance` 2503) input
  print
    . maximum
    . foldl' (\acc x -> zipWith (+) (awardPoint x) acc) (repeat 0)
    $ map (\t -> map (`distance` t) input) [1 .. 2503]
