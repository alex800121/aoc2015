module Day22 (day22) where

import MyLib
import Data.List
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import Data.Set (Set)
import qualified Data.Set as Set

data Boss = B { bHit :: Int, bDamage :: Int } deriving (Show, Eq, Ord)

initBoss :: Boss
initBoss = B 51 9

type Spell = [(Int, Int, Int, Int)] -- (manna, hit, damage, armor)
type Spells = [(Int, Spell, Int, Int)]
data Player = 
  P { pManna :: Int
    , pHit :: Int
    , pDamage :: Int
    , pArmor :: Int
    , pSpells :: Spell
    }
  deriving (Show, Eq, Ord)


initPlayer :: Player
initPlayer = P 500 50 0 0 []

data GameState = G { accumManna :: Int, player :: Player, boss :: Boss, spellCandidates :: Spells } deriving (Show, Eq, Ord)

initGameState :: GameState
initGameState = G 0 initPlayer initBoss spells

testGameState :: GameState
testGameState = G 0 (P 250 10 0 0 []) (B 13 8) spells

testGameState2 :: GameState
testGameState2 = G 0 (P 250 10 0 0 []) (B 14 8) spells

spells :: Spells
spells =
  [ (53, [(0, 0, 4, 0), (0, 0, -4, 0)], 1, 0)
  , (73, [(0, 2, 2, 0), (0, 0, -2, 0)], 1, 0)
  , (113, [(0, 0, 0, 0), (0, 0, 0, 7), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, -7)], 6, 0)
  , (173, [(0, 0, 0, 0), (0, 0, 3, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, -3, 0)], 6, 0)
  , (229, [(0, 0, 0, 0), (110, 0, 0, 0), (110, 0, 0, 0), (110, 0, 0, 0), (110, 0, 0, 0), (110, 0, 0, 0)], 5, 0)
  ]

addSpell :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addSpell (x1, x2, x3, x4) (y1, y2, y3, y4) = (x1 + y1, x2 + y2, x3 + y3, x4 + y4)

zipSpells :: Spell -> Spell -> Spell
zipSpells [] ys = ys
zipSpells xs [] = xs
zipSpells (x : xs) (y : ys) =
  addSpell x y : zipSpells xs ys

chooseSpells :: [(Int, Spell)] -> Player -> [(Int, Player)]
chooseSpells ls (P m h d a s) =
  [ (cost, P m' h' d' a' xs)
  | (cost, l) <- ls
  , cost <= m
  , let s' = zipSpells s l
  , let (x, xs) = fromMaybe ((0, 0, 0, 0), []) (uncons s')
  , let (m', h', d', a') = addSpell (m - cost, h, d, a) x
  ]

day22 :: IO ()
day22 = do
  -- putStrLn $ ("day22a: " ++) $ show $ (!! 2) $ iterate (concatMap (step spells)) [initGameState]
  -- putStrLn $ ("day22a: " ++) $ show $ (!! 2) $ iterate (concatMap (step spells)) [testGameState]
  putStrLn $ ("day22b: " ++) $ show $ ""