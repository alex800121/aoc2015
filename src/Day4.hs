{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Day4 where

import "cryptohash-md5" Crypto.Hash.MD5
import qualified Crypto.Hash as H
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.String (IsString(..))
import Data.List (find)
import Data.Maybe (fromJust)
import Crypto.Hash (MD5)

input =  "yzbqklnj"

prefix5 =  "00000"

prefix6 =  "000000"

withPostFix :: ByteString -> ByteString -> ByteString
withPostFix p x = H.digestToHexByteString  $ fromJust $ H.digestFromByteString @MD5  $ hash (B.append x p)

day4 :: IO ()
day4 = do
  print $ find (B.isPrefixOf prefix5 . (`withPostFix` input) . fromString . show) [1..]
  print $ find (B.isPrefixOf prefix6 . (`withPostFix` input) . fromString . show) [1..]
