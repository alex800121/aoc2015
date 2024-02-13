module Day23 where

import Data.Vector (Vector)
import qualified Data.Vector as V
import MyLib
import Text.Megaparsec (parseMaybe)
import Data.Maybe (fromJust)

type S = (Int, (Int, Int))

type Ins = Vector [String]

readIns :: Ins -> S -> S
readIns v s@(p, (a, b)) = case v V.!? p of
  Nothing -> s
  Just ["jio", x, y] -> undefined
  where
    readReg x = if head x == 'a' then fst else snd
    readInt = fromJust . parseMaybe signedInteger

day23 :: IO ()
day23 = do
  input <- V.fromList . map words . lines <$> readFile "input/input23.txt"
  print input
