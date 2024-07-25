module Day13 where

import Paths_AOC2015
import Data.List (nub, permutations, sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

type Name = String

type Rule = Map (Name, Name) Int

-- Alice would lose 57 happiness units by sitting next to Bob.
inputParser :: String -> Rule
inputParser s
  | [name0, _, gl, n, _, _, _, _, _, _, name1] <- words s,
    name1' <- init name1 =
      Map.singleton (min name0 name1', max name0 name1') ((if gl == "lose" then negate else id) (read n))

calc :: Rule -> [String] -> [Int]
calc r xs = sort $ f xs'
  where
    xs' = last xs : xs
    f (x : y : ys) = (r Map.! (min x y, max x y)) : f (y : ys)
    f _ = []

day13 :: IO ()
day13 = do
  input <- Map.unionsWith (+) . map inputParser . lines <$> (getDataDir >>= readFile . (++ "/input/input13.txt"))
  let names = nub $ concatMap ((++) <$> pure . fst <*> pure . snd) $ Map.keys input
      p = permutations names
      l = map (calc input) p
  print $ maximum $ map sum l
  print $ maximum $ map (sum . tail) l
