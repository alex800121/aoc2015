{-# LANGUAGE TupleSections #-}

module Day21 where

import Paths_AOC2015
import Control.Lens (bimap)
import MyLib (pick)
import Data.Function (on)
import Data.List (sortBy, find)

weapons = zip [8, 10, 25, 40, 74] $ map (,0) [4 .. 8]

armor = zip [13, 31, 53, 75, 102] $ map (0,) [1 .. 5]

rings = zip [25, 50, 100] (map (,0) [1 .. 3]) <> zip [20, 40, 80] (map (0,) [1 .. 3])

buy :: [(Int, (Int, Int))]
buy = do
  w <- weapons
  n0 <- [0, 1]
  a <- pick n0 armor
  n1 <- [0 .. 2]
  r <- pick n1 rings
  pure $ foldr (\(x, (y, z)) -> bimap (+ x) (bimap (+ y) (+ z))) (0, (0, 0)) (w : a ++ r)

data Player = P
  { _role :: Bool,
    _hp :: Int,
    _damage :: Int,
    _armor :: Int
  }
  deriving (Show, Eq, Ord)

buildPlayer :: Bool -> Int -> (Int, (Int, Int)) -> Player
buildPlayer role hp (_, (damage, armor)) = P role hp damage armor

play :: Player -> Player -> Bool
play (P r0 h0 d0 a0) (P r1 h1 d1 a1)
  | h0 <= 0 = r1
  | h1 <= 0 = r0
  | otherwise = play (P r1 h1' d1 a1) (P r0 h0 d0 a0)
  where
    damageDealt = max 1 $ d0 - a1
    h1' = h1 - damageDealt

day21 :: IO ()
day21 = do
  boss <- (\[x, y, z] -> P False x y z) . map (read @Int . last . words) . lines <$> (getDataDir >>= readFile . (++ "/input/input21.txt"))
  print $ fmap fst $ find ((`play` boss) . buildPlayer True 100) $ sortBy (compare `on` fst) buy
  print $ fmap fst $ find (not . (`play` boss) . buildPlayer True 100) $ sortBy (flip compare `on` fst) buy
