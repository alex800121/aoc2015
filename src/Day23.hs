{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day23 (day23) where

import MyLib
import Data.Maybe (fromJust)
import Text.Megaparsec ( anySingle, parseMaybe, parseTest )
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad.Combinators ( (<|>) )
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (when)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
-- import Debug.Trace
import Control.Effect.State.Labelled
import Control.Effect.Labelled
import Control.Carrier.State.Strict hiding (get, put)
import Control.Carrier.Writer.Strict
import Control.Effect.Writer
import Control.Effect.Reader
import Control.Carrier.Reader
import GHC.Natural (Natural)
import Data.Monoid (Sum)
import Debug.Trace
import Control.Monad (guard)
import Data.List ((\\))

type Registry = Map Reg Int
type Reg = Char
data Reg2 = Re Reg | In Int deriving Show
data RegIns = Set Reg Reg2
            | Sub Reg Reg2
            | Mul Reg Reg2
            | Jnz Reg2 Reg2
            deriving Show

data Player = Player {registry :: Registry, position :: Int} deriving Show

playera = Player Map.empty 0
player0 = Player (Map.singleton 'p' 0) 0
player1 = Player (Map.singleton 'p' 1) 0
playera' = Player (Map.singleton 'a' 1) 0

runPlayer :: forall n sig m. (HasLabelled (n :: Natural) (State Player) sig m) => [RegIns] -> m Int
runPlayer regIns = do
  player <- get @n
  -- case trace (show player) $ regIns !? player.position of
  case regIns !? player.position of
    Nothing -> return 0
    Just ins -> case ins of
      Set reg reg2 -> let
        player' = player {registry = Map.insert reg (interpretReg player.registry reg2) player.registry, position = player.position + 1}
        in put @n player' >> runPlayer @n regIns
      Sub reg reg2 -> let
        player' = player {registry = Map.adjust (subtract (interpretReg player.registry reg2)) reg player.registry, position = player.position + 1}
        in put @n player' >> runPlayer @n regIns
      Mul reg reg2 -> let
        player' = player {registry = Map.adjust (* interpretReg player.registry reg2) reg player.registry, position = player.position + 1}
        in put @n player' >> (+ 1) <$> runPlayer @n regIns
      Jnz reg1 reg2 -> let
        x = interpretReg player.registry reg1
        offset = if x /= 0 then interpretReg player.registry reg2 else 1
        player' = player {position = player.position + offset}
        in put @n player' >> runPlayer @n regIns



interpretReg :: Registry -> Reg2 -> Int
interpretReg _ (In int) = int
interpretReg m (Re reg) = Map.findWithDefault 0 reg m
  
regParser :: Parser Reg2
regParser = (In <$> signedInteger) <|> (Re <$> anySingle)

inputParser :: Parser RegIns
inputParser =
      (Set <$> (string "set " >> anySingle) <*> (space >> regParser))
  <|> (Sub <$> (string "sub " >> anySingle) <*> (space >> regParser))
  <|> (Mul <$> (string "mul " >> anySingle) <*> (space >> regParser))
  <|> (Jnz <$> (string "jnz " >> regParser) <*> (space >> regParser))

b :: [Int]
b = takeWhile (<= 106700 + 17000) $ iterate (+ 17) 106700

prime :: [Int]
prime = f [2..]
  where
    f (x : xs) = x : filter ((/= 0) . (`mod` x)) (f xs)

isPrime :: Int -> Bool
isPrime k = k > 1 && null [ x | x <- [2..isqrt k], k `mod` x == 0]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

day23b = filter (not . isPrime) b

day23 :: IO ()
day23 = do
  ins <- map (fromJust . parseMaybe inputParser) . lines <$> readFile "input23.txt"
  putStrLn ("day23a: " ++ show (run $ runState playera $ runLabelled @0 $ runPlayer @0 ins))
  putStrLn ("day23b: " ++ show (length day23b))