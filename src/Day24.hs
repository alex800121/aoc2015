
module Day24 (day24) where

import MyLib hiding (Tree)
import Data.List (partition, delete, maximumBy)
import Data.Function (on)
import Data.List.Split (splitOn)
import Control.Monad (guard)
-- import Data.Set (Set)
-- import qualified Data.Set as Set

newtype Bridge = B (Int, Int) deriving (Show, Eq)

inBridge :: Int -> Bridge -> Bool
inBridge i (B b) = i == fst b || i == snd b

toNext :: Int -> Bridge -> Maybe Int
toNext i (B (x, y))
  | i == x = Just y
  | i == y = Just x
  | otherwise = Nothing

canConnect :: Bridge -> Bridge -> Bool
canConnect (B b1) b2 = inBridge (fst b1) b2 || inBridge (snd b1) b2

inputParser :: String -> [Bridge]
inputParser = map ((\(x : y : _) -> B (x, y)) . map (read @Int) . splitOn "/") . lines

buildBridges :: [Bridge] -> Int -> [[Bridge]]
buildBridges b i = let (candidates, rest) = partition (inBridge i) b in if null candidates then return [] else do
  nextBridge <- candidates
  let 
    Just i' = toNext i nextBridge
    b' = delete nextBridge b
  (nextBridge :) <$> buildBridges b' i'

scoreBridge :: Bridge -> Int
scoreBridge (B b) = uncurry (+) b

scoreBridges :: [Bridge] -> Int
scoreBridges = sum . map scoreBridge

day24a :: [[Bridge]] -> Int
day24a = maximum . map scoreBridges

day24b :: [[Bridge]] -> Int
day24b = scoreBridges . maximumBy ((compare `on` length) <> (compare `on` scoreBridges))

day24 :: IO ()
day24 = do
  bridges <- inputParser <$> readFile "input24.txt"
  let bridgesFrom0 = buildBridges bridges 0
  putStrLn ("day24a: " ++ show (day24a bridgesFrom0))
  putStrLn ("day24b: " ++ show (day24b bridgesFrom0))