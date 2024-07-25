module Day9 where

import Paths_AOC2015
import Data.Function (on)
import Data.List (nub, permutations)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import MyLib (pickAnySplit)

type Road = Map V Int

type V = (String, String)

parseRoad :: String -> Road
parseRoad s = Map.singleton (x0, x1) (read y)
  where
    [x0, _, x1, _, y] = words s

getDistance :: Road -> V -> Int
getDistance r v = Map.findWithDefault (getDistance r (swap v)) v r

day9 :: IO ()
day9 = do
  input <- Map.unions . map parseRoad . lines <$> (getDataDir >>= readFile . (++ "/input/input9.txt"))
  let vs =
        map (sum . (zipWith (curry (getDistance input)) <$> id <*> tail))
          . permutations
          . nub
          . concatMap (uncurry ((++) `on` (: [])))
          $ Map.keys input
  print $ minimum vs
  print $ maximum vs
