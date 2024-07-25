module Day8 where

import Paths_AOC2015
import Data.Char
import MyLib (Parser, signedInteger)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Bifunctor (Bifunctor(..))

readSlash :: Parser Char
readSlash = string "\\\\" >> pure '\\'

readQuote :: Parser Char
readQuote = string "\\\"" >> pure '\"'

readHex :: Parser Char
readHex = string "\\x" >> f <$> count 2 (satisfy isHexDigit)
  where
    f = chr . foldl' (\acc x -> digitToInt x + (16 * acc)) 0

readASCII :: Parser String
readASCII = char '\"' >> many (readSlash <|> readQuote <|> readHex <|> anySingleBut '\"') <* char '\"'

encodeASCII :: String -> String
encodeASCII = show
day8 :: IO ()
day8 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input8.txt"))
  -- input <- lines <$> readFile "input/test8.txt"
  print $ uncurry (-) $ bimap sum sum $ unzip $ map ((,) <$> length <*> length . fromMaybe "" . parseMaybe readASCII) input
  print $ uncurry subtract $ bimap sum sum $ unzip $ map ((,) <$> length <*> length . encodeASCII) input
