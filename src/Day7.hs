module Day7 where

import Paths_AOC2015
import Control.Monad.Trans.State.Strict
import Data.Bits
import Data.Char (isAlpha)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Word
import MyLib
import Text.Megaparsec (many, parseMaybe, satisfy, (<|>))
import Text.Megaparsec.Char

type Reg = Map String Op

type Ans = Map String Word16

data Op = I Word16 | R String | NOT Op | AND Op Op | OR Op Op | RSHIFT Op Op | LSHIFT Op Op
  deriving (Show, Eq, Ord)

solve :: Op -> Reg -> State Ans Word16
solve (I x) r = pure x
solve (R s) r = do
  ans <- get
  case ans Map.!? s of
    Just x -> pure x
    Nothing -> do
      x <- solve (r Map.! s) r
      modify' (Map.insert s x)
      pure x
solve (NOT x) r = xor maxBound <$> solve x r
solve (AND x y) r = (.&.) <$> solve x r <*> solve y r
solve (OR x y) r = (.|.) <$> solve x r <*> solve y r
solve (RSHIFT x y) r = shiftR <$> solve x r <*> (fromIntegral <$> solve y r)
solve (LSHIFT x y) r = shiftL <$> solve x r <*> (fromIntegral <$> solve y r)

parseWord :: Parser Word16
parseWord = fromIntegral <$> signedInteger

parseUnit = (I <$> parseWord) <|> (R <$> many (satisfy isAlpha)) 

parse = fromJust . parseMaybe parseUnit

opParser :: String -> Op
opParser s = case words s of
  [x] -> parse x
  ["NOT", x] -> NOT $ parse x
  [x, "AND", y] -> AND (parse x) (parse y)
  [x, "OR", y] -> OR (parse x) (parse y)
  [x, "LSHIFT", y] -> LSHIFT (parse x) (parse y)
  [x, "RSHIFT", y] -> RSHIFT (parse x) (parse y)

day7 :: IO ()
day7 = do
  input <- Map.fromList . map ((\[x, y] -> (y, opParser x)) .  splitOn " -> ") . lines <$> (getDataDir >>= readFile . (++ "/input/input7.txt"))
  let a = evalState (solve (R "a") input) Map.empty
  print $ a
  print $ evalState (solve (R "a") (Map.insert "b" (I a) input)) Map.empty
