module Day12 where

import Paths_AOC2015
import Data.Bifoldable
import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char

data JSON a b
  = St a
  | Nu b
  | Ar [JSON a b]
  | Ob [(String, JSON a b)]
  deriving (Eq, Ord, Show)

instance Bifoldable JSON where
  bifoldr fa _ c (St a) = fa a c
  bifoldr _ fb c (Nu b) = fb b c
  bifoldr fa fb c (Ar ls) = foldr (flip (bifoldr fa fb)) c ls
  bifoldr fa fb c (Ob ls) = foldr (flip (bifoldr fa fb) . snd) c ls

jsonParser, stParser, nuParser, arParser, obParser :: Parser (JSON String Int)
jsonParser =
  stParser
    <|> nuParser
    <|> arParser
    <|> obParser
stParser = char '\"' >> St <$> many (anySingleBut '\"') <* char '\"'
nuParser = Nu <$> signedInteger
arParser = char '[' >> (Ar <$> (jsonParser `sepBy` char ',')) <* char ']'
obParser = char '{' >> (Ob <$> (((,) <$> (char '\"' >> many (anySingleBut '\"') <* char '\"') <*> (char ':' >> jsonParser)) `sepBy` char ',')) <* char '}'

day12b :: JSON String Int -> Int
day12b (St _) = 0
day12b (Nu a) = a
day12b (Ar ls) = sum (map day12b ls)
day12b (Ob ls) = if any ((== St "red") . snd) ls then 0 else sum (map (day12b . snd) ls)

day12 :: IO ()
day12 = do
  Just input <- parseMaybe (jsonParser <* newline) <$> (getDataDir >>= readFile . (++ "/input/input12.txt"))
  print $ bifoldr (const id) (+) 0 input
  print $ day12b input
