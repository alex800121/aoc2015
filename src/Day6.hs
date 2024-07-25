{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Day6 where

import Paths_AOC2015
import Control.Monad.ST.Strict (ST, runST)
import qualified Data.Array.MArray as M
import qualified Data.Array.ST as S
import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as U
import Data.Bifunctor (Bifunctor (..))
import Data.Maybe (mapMaybe)
import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char


type M s a = S.STUArray s Index a

type Index = (Int, Int)

type Range = (Index, Index)

data InsType = On | Off | Toggle deriving (Show, Eq, Ord)

type Ins = (InsType, Range)



initArray :: a -> U.Array Index a
initArray x = U.listArray initRange (replicate (1000 * 1000) x)

initRange = ((0, 0), (999, 999))


insParser :: Parser Ins
insParser = do
  s <- (string "turn on " >> pure On) <|> (string "turn off " >> pure Off) <|> (string "toggle " >> pure Toggle)
  i <- (,) <$> signedInteger <*> (char ',' >> signedInteger)
  string " through "
  j <- (,) <$> signedInteger <*> (char ',' >> signedInteger)
  pure (s, (i, j))

run :: (forall s. M.MArray (S.STUArray s) a (ST s)) => (InsType -> a -> a) -> [Ins] -> U.Array Index a -> U.Array Index a
run g ins a = runST $ do
  a' <- M.thaw a
  mapM_ (\x -> readIns g x a') ins
  M.freeze a'

readIns :: (M.MArray (S.STUArray s) a (ST s)) => (InsType -> a -> a) -> Ins -> M s a -> ST s ()
readIns g (s, r) a = mapM_ (\i -> M.readArray a i >>= M.writeArray a i . g s) (U.range r)

day6a On = const True
day6a Off = const False
day6a Toggle = not

day6b On = (+ 1)
day6b Off = max 0 . subtract 1
day6b Toggle = (+ 2)

day6 :: IO ()
day6 = do
  input <- mapMaybe (parseMaybe insParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input6.txt"))
  print $ length $ filter id $ U.elems $ run day6a input (initArray False)
  print $ sum $ run day6b input (initArray (0 :: Int))
