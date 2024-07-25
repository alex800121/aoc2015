{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}

module Day19 where

import Paths_AOC2015
import Control.Applicative.Combinators ((<|>))
import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isLower, isUpper)
import Data.Kind (Type)
import Data.List (inits, isPrefixOf, nub, tails)
import Data.List.Split (splitOn, startsWithOneOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Debug.Trace (traceShow)
import qualified Text.Megaparsec as T
import Text.ParserCombinators.ReadP
import Text.Read (Read (..), lift)

type NParser = T.Parsec Void [NewString]

type Replace = Map String [String]

type ReplaceNew = Map [NewString] NewString

ruleParser :: ReplaceNew -> NParser NewString
ruleParser rule = T.choice $ map (\(x, y) -> T.chunk x >> pure y) (Map.toList rule)

data NewStringF f
  = StF {_stf :: String}
  | RnYArF {_contentf :: [f (NewStringF f)]}

type NewString' = NewStringF []
deriving instance Show NewString'
deriving instance Eq NewString'
deriving instance Ord NewString'

data NewString
  = St {_st :: String}
  | RnYAr {_content :: [[NewString]]}
  deriving (Eq, Show, Ord)

instance Read NewString where
  readPrec = lift newStringParser

calc :: [NewString] -> Int
calc [] = -1
calc (St _ : xs) = 1 + calc xs
calc (RnYAr ls : xs) = sum (map calc ls) + 1 + calc xs

newStringParser :: ReadP NewString
newStringParser = stParser <|> rnyarParser

rnyarParser :: ReadP NewString
rnyarParser =
  between (string "Rn") (string "Ar") (RnYAr <$> (many newStringParser `sepBy` char 'Y'))

stParser :: ReadP NewString
stParser = do
  s <- look
  if any (`isPrefixOf` s) ["Rn", "Ar", "Y"] then pfail else St <$> ((:) <$> satisfy isUpper <*> munch isLower)

buildParser :: Replace -> ReadP String
buildParser r = choice rs
  where
    rs = do
      (x, xs) <- Map.toList r
      y <- xs
      pure (string x >> return y)

parseOnce :: Replace -> String -> [String]
parseOnce rule s =
  map (uncurry (<>))
    . readP_to_S ((<>) <$> choice (map (`count` get) [0 .. length s]) <*> buildParser rule)
    $ s

parse :: String -> [NewString]
parse = fst . head . filter (null . snd) . readP_to_S (many newStringParser)

day19 :: IO ()
day19 = do
  (rule, s) <-
    ( \[a, b] ->
        ( ( Map.unionsWith (<>)
              . map ((\[x, y] -> Map.singleton x [y]) . splitOn " => ")
              . lines
          )
            a,
          init b
        )
      )
      . splitOn "\n\n"
      <$> (getDataDir >>= readFile . (++ "/input/input19.txt"))
  let reverseRule =
        Map.unions [Map.singleton (parse x) (St y) | (y, b) <- Map.toList rule, x <- b]
      a = parse s
  print
    . length
    . Set.fromList
    . parseOnce rule
    $ s
  print $ calc a
