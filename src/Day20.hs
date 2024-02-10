module Day20 where

import Data.List (find, findIndex, nub)
import MyLib (sqrtCeiling)

input = 36000000 :: Int

factors :: Integral a => a -> [a]
factors x = nub $ concat $ [[a, b] | a <- [1 .. y], let (b, m) = x `divMod` a, m == 0]
  where
    y = sqrtCeiling x

day20 :: IO ()
day20 = do
  let houseGifts = map ((* 10) . sum . factors) [0 ..]
      houseGifts' = map (\y -> (* 11) . sum . filter ((<= 50) . (y `div`)) . factors $ y) [0 ..]
  print $ findIndex (>= input) houseGifts
  print $ findIndex (>= input) houseGifts'

-- print $ take 1000 $ zip [0..] houseGifts
