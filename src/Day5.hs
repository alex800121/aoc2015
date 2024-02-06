module Day5 where
import Data.List (isInfixOf, group)
import Data.List.Split (divvy)

niceA :: String -> Bool
niceA s = containVowelN 3 s && not (any (`isInfixOf` s) disallowed) && any ((>= 2) . length) (group s)

containVowelN :: Int -> String -> Bool
containVowelN n s = length (filter (`elem` vowels) s) >= n

vowels = "aeiou"

disallowed = ["ab", "cd", "pq", "xy"]

niceB :: String -> Bool
niceB s = pairs s && threes
  where
    pairs (x : y : xs) = ([x, y] `isInfixOf` xs) || pairs (y : xs)
    pairs _ = False
    threes = any ((==) <$> head <*> last) $ divvy 3 1 s
day5 :: IO ()
day5 = do
  input <- lines <$> readFile "input/input5.txt"
  print $ length $ filter niceA input
  print $ length $ filter niceB input
