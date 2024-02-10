module Day11 where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (chr, ord)
import Data.List (foldl', unfoldr, find)
import Data.List.Split (divvy)
import Data.String (IsString (..))
import Data.Tuple (swap)

newtype PW = PW {_pw :: Int} deriving (Eq, Ord)

instance IsString PW where
  fromString = PW . foldl' f 0
    where
      f acc c = (acc * 26) + (ord c - ord 'a')

instance Enum PW where
  toEnum = PW
  fromEnum = _pw

instance Show PW where
  show = f "" . _pw
    where
      f acc x
        | x <= 0 = acc
        | otherwise = let (d, m) = divMod x 26 in f (chr (m + ord 'a') : acc) d

input = "cqjxjnds"

pass :: PW -> Bool
pass p = any f s0 && not (any (`elem` "iol") s) && g 2 s
  where
    s = show p
    s0 = divvy 3 1 s
    f [x, y, z] = z == succ y && y == succ x
    g n _ | n <= 0 = True
    g n (x : y : xs)
      | x == y = g (n - 1) xs
      | otherwise = g n (y : xs)
    g _ _ = False

day11 :: IO ()
day11 = do
  -- input <- readFile "input/input11.txt"
  print $ find pass $ iterate succ $ fromString input
  print $ find pass $ tail $ dropWhile (not . pass) $ iterate succ $ fromString input
