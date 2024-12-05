module Day23 where

import Control.Lens
import Data.List (unfoldr)
import Data.Maybe (fromJust, fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import MyLib
import Text.Megaparsec (parseMaybe)
import Paths_AOC2015

type S = (Int, (Int, Int))

type Ins = Vector [String]

readIns :: Ins -> S -> Maybe S
readIns v s = f <$> v V.!? view _1 s
  where
    readReg x = _2 . (if head x == 'a' then _1 else _2)
    readInt = fromJust . parseMaybe signedInteger
    f ["hlf", x] = over _1 (+ 1) $ over (readReg x) (`div` 2) s
    f ["tpl", x] = over _1 (+ 1) $ over (readReg x) (* 3) s
    f ["inc", x] = over _1 (+ 1) $ over (readReg x) (+ 1) s
    f ["jmp", y] = over _1 (+ readInt y) s
    f ["jie", x, y] = over _1 (+ (if even (view (readReg x) s) then readInt y else 1)) s
    f ["jio", x, y] = over _1 (+ (if view (readReg x) s == 1 then readInt y else 1)) s

go f x = maybe x (go f) (f x)

day23 :: IO ()
day23 = do
  input <- V.fromList . map words . lines <$> (readFile . (++ "/input/input23.txt") =<< getDataDir)
  print $ snd . snd $ go (readIns input) (0, (0, 0))
  print $ snd . snd $ go (readIns input) (0, (1, 0))
