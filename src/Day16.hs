module Day16 where

import Data.Foldable (find)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import MyLib (Parser, signedInteger)
import Paths_AOC2015
import Text.Megaparsec
import Text.Megaparsec.Char

type Aunt = IntMap [Gift]

type Gift = (String, Int)

type Received = Map String Int

gifts :: Received
gifts =
  Map.fromList
    . map ((\[x, y] -> (x, read y)) . splitOn ": ")
    $ lines "children: 3\ncats: 7\nsamoyeds: 2\npomeranians: 3\nakitas: 0\nvizslas: 0\ngoldfish: 5\ntrees: 3\ncars: 2\nperfumes: 1"

-- Sue 1: children: 1, cars: 8, vizslas: 7
auntParser :: Parser Aunt
auntParser = do
  n <- string "Sue " >> signedInteger <* char ':' <* space
  g <- ((,) <$> (many (anySingleBut ':') <* string ": ") <*> signedInteger) `sepBy` string ", "
  pure $ IM.singleton n g

day16 :: IO ()
day16 = do
  input <- IM.unions . mapMaybe (parseMaybe auntParser) . lines <$> (readFile . (++ "/input/input16.txt") =<< getDataDir)
  print $ fst $ IM.findMin $ IM.filter (all (\(x, y) -> gifts Map.!? x == Just y)) input
  print
    . fst
    . IM.findMin
    $ IM.filter (all f) input

f (x, y)
  | x `elem` ["cats", "trees"] = gifts Map.! x < y
  | x `elem` ["pomeranians", "goldfish"] = gifts Map.! x > y
  | otherwise = gifts Map.! x == y
