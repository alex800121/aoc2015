{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Day22 where

import Control.Lens
import Data.Bifunctor (Bifunctor (..))
import Data.Either (fromRight)
import Data.List (foldl')
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.PQueue.Prio.Min (MinPQueue (..))
import qualified Data.PQueue.Prio.Min as PQ
import Debug.Trace (traceShow)

data Instant
  = Missile
  | Drain
  deriving (Show, Eq, Ord, Bounded, Enum)

data Effect
  = Shield
  | Poison
  | Recharge
  deriving (Show, Eq, Ord, Bounded, Enum)

type Spell = Either Instant (Effect, Int)

data GameState = G
  { _turn :: Bool,
    _player :: Player,
    _boss :: Boss,
    _effects :: MultiSet Effect
  }
  deriving (Show, Eq, Ord)

data Player = Player
  { _playerHp :: Int,
    _manna :: Int
  }
  deriving (Show, Eq, Ord)

data Boss = Boss
  { _bossHp :: Int,
    _damage :: Int
  }
  deriving (Show, Eq, Ord)

makeLenses ''Instant
makeLenses ''Effect
makeLenses ''Player
makeLenses ''Boss
makeLenses ''GameState

type Q = MinPQueue Int GameState

initBoss = Boss 71 10

initPlayer = Player 50 500

initGameState = G True initPlayer initBoss MS.empty

initQ = PQ.singleton 0 initGameState

-- step :: (Int, GameState) -> (Int, GameState)
-- step g = (if view (_2 . turn) g then castSpell else bossTurn) $ over _2 spellEffect g
-- cost :: Spell -> Int
-- cost (Left Missile) = 53
-- cost (Left Drain) = 73
-- cost (Right Shield) = 113
-- cost (Right Poison) = 173
-- cost (Right Recharge) = 229
allSpells :: [(Spell, Int)]
allSpells =
  [ (Left Missile, 53),
    (Left Drain, 73),
    (Right (Shield, 6), 113),
    (Right (Poison, 6), 173),
    (Right (Recharge, 5), 229)
  ]

castSpell :: Int -> GameState -> [(Int, GameState)]
castSpell initN g =
  [ (initN + n, either readInstant fr x g')
    | (x, n) <- canCast,
      let g' = over (player . manna) (subtract n) g
  ]
  where
    fr = over effects . uncurry MS.insertMany
    canCast =
      filter
        ( (&&)
            <$> (<= view (player . manna) g) . snd
            <*> either (const True) ((`MS.notMember` view effects g) . fst) . fst
        )
        allSpells

bossTurn, spellEffect :: GameState -> GameState
spellEffect g = foldl' (\acc s -> readEffect s $ over effects (MS.delete s) acc) g es
  where
    es = MS.distinctElems $ view effects g
bossTurn g = over (player . playerHp) (subtract damageDealt) g
  where
    damageDealt = max 1 $ view (boss . damage) g - (if MS.member Shield (view effects g) then 7 else 0)

readEffect :: Effect -> GameState -> GameState
readEffect Shield = id
readEffect Poison = over (boss . bossHp) (subtract 3)
readEffect Recharge = over (player . manna) (+ 101)

readInstant :: Instant -> GameState -> GameState
readInstant Missile = over (boss . bossHp) (subtract 4)
readInstant Drain = over (player . playerHp) (+ 2) . over (boss . bossHp) (subtract 2)

dijkstra :: Int -> Q -> Maybe (Int, GameState)
dijkstra _ PQ.Empty = Nothing
dijkstra i ((n, g) PQ.:< gs)
  -- | traceShow (n, g) False = undefined
  | view (player . playerHp) g' <= 0 = dijkstra i gs
  | view (boss . bossHp) g' <= 0 = Just (n, g')
  | view (boss . bossHp) g'' <= 0 = Just (n, g'')
  | view turn g = dijkstra i $ PQ.union gs gps
  | otherwise = dijkstra i $ PQ.insert n gb gs
  where
    g' = over (player . playerHp) (subtract $ if view turn g then i else 0) g
    g'' = spellEffect $ over turn not g'
    gps = PQ.fromList $ castSpell n g''
    gb = bossTurn g''

testGameState = G True (Player 10 250) (Boss 13 8) MS.empty
day22 :: IO ()
day22 = do
  -- input <- readFile "input/input22.txt"
  print $ fmap fst $ dijkstra 0 $ PQ.singleton 0 initGameState
  print $ fmap fst $ dijkstra 1 $ PQ.singleton 0 initGameState
  -- print $ dijkstra $ PQ.singleton 0 testGameState
  -- print $ dijkstra $ PQ.singleton 0 $ over (boss . bossHp) (const 14) testGameState
