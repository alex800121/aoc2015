module Day18 where

import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as U
import Data.Bifunctor (Bifunctor (..))
import Data.Maybe (mapMaybe)
import MyLib (drawArray)

type Lights = Array Index Bool

type Index = (Int, Int)

surrounding = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

(!?) :: (U.IArray a e, U.Ix i) => a i e -> i -> Maybe e
a !? i
  | U.inRange (U.bounds a) i = Just (a U.! i)
  | otherwise = Nothing

step' :: Lights -> Lights
step' a = a'
  where
    a' =
      U.array
        b
        [ (i, f i)
          | i <- U.range b
        ]
    b = U.bounds a
    corners = [(x, y) | x <- [fst (fst b), fst (snd b)], y <- [snd (fst b), snd (snd b)]]
    f i
      | i `elem` corners = True
      | e, xs `elem` [2, 3] = True
      | e = False
      | xs == 3 = True
      | otherwise = False
      where
        e = a U.! i
        xs = length $ filter id $ mapMaybe ((a !?) . bimap (+ fst i) (+ snd i)) surrounding

step :: Lights -> Lights
step a = a'
  where
    a' =
      U.array
        b
        [ (i, f i)
          | i <- U.range b
        ]
    b = U.bounds a
    f i
      | e, xs `elem` [2, 3] = True
      | e = False
      | xs == 3 = True
      | otherwise = False
      where
        e = a U.! i
        xs = length $ filter id $ mapMaybe ((a !?) . bimap (+ fst i) (+ snd i)) surrounding

day18 :: IO ()
day18 = do
  input <- U.amap (== '#') . drawArray @Array . lines <$> readFile "input/input18.txt"
  print
    . length
    . filter id
    . U.elems
    . (!! 100)
    $ iterate step input
  print
    . length
    . filter id
    . U.elems
    . (!! 100)
    $ iterate step' input
