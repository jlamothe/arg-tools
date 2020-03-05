{-|

ARG Tools
Copyright (C) 2019 Jonathan Lamothe
<jlamothe1980@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

-}

{-# LANGUAGE LambdaCase #-}

module ARGTools
  ( fromHex
  , toHex
  , fromUTF8
  , toUTF8
  , toBinary
  , toBits
  , caesar
  , polyAlpha
  ) where

import qualified Data.ByteString.Base16.Lazy as B16
import Data.ByteString.Builder (toLazyByteString, stringUtf8)
import qualified Data.ByteString.Lazy as BS
import Data.Char (chr, isAsciiLower, isAsciiUpper, ord)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8')
import Data.Word (Word8)
import Numeric (showIntAtBase)

-- | Decodes a hexadecimal string
fromHex :: String -> Maybe BS.ByteString
fromHex = do
  (res, err) <- B16.decode . toUTF8
  if BS.null err
    then return $ Just res
    else return Nothing

-- | Encodes a bytestring to hex
toHex :: BS.ByteString -> String
toHex = fromJust . fromUTF8 . B16.encode

-- | Decodes a UTF8 string
fromUTF8 :: BS.ByteString -> Maybe String
fromUTF8 = decodeUtf8' >>= \case
  Left _    -> return Nothing
  Right res -> return $ Just $ T.unpack res

-- | Encodes a string to UTF8
toUTF8 :: String -> BS.ByteString
toUTF8 = toLazyByteString . stringUtf8

-- | Encodes a ByteString into binary
toBinary :: BS.ByteString -> [String]
toBinary = map toBits . BS.unpack

-- | Converts a byte to bits
toBits :: Word8 -> String
toBits n = let
  str  = showIntAtBase 2 (chr . (ord '0' +)) n ""
  sLen = length str
  pLen = 8 - sLen
  pad  = replicate pLen '0'
  in pad ++ str

-- | Applies a Caesar cypher to a string
caesar
  :: Int
  -- ^ The shift
  -> String
  -> String
caesar n = polyAlpha [n] False

-- | Applies a polyalphabetic cypher
polyAlpha
  :: [Int]
  -- ^ The key
  -> Bool
  -- ^ Set 'True' when we should only iterate through the key on
  -- alphabetic characters
  -> String
  -> String
polyAlpha []   = \_ str -> str
polyAlpha keys =
  subFunc keys'
  where
    keys' = concat $ repeat keys

    subFunc _ _ [] = []
    subFunc [] _ str = str
    subFunc kks@(k:ks) alphaIter (c:cs) = let
      (isAlpha, c') = shift k c
      in c' : if isAlpha || not alphaIter
        then subFunc ks alphaIter cs
        else subFunc kks alphaIter cs

    shift k c = let
      start
        | isAsciiLower c = Just 'a'
        | isAsciiUpper c = Just 'A'
        | otherwise      = Nothing

      shiftFrom s = chr $
        (ord c - ord s + k) `mod` 26 + ord s

      in case start of
        Just s  -> (True, shiftFrom s)
        Nothing -> (False, c)

-- jl
