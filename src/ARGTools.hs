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

module ARGTools
  ( fromHex
  , toUTF8
  ) where

import qualified Data.ByteString.Base16.Lazy as B16
import Data.ByteString.Builder (toLazyByteString, stringUtf8)
import qualified Data.ByteString.Lazy as BS

-- | Decodes a hexadecimal string
fromHex :: String -> Maybe BS.ByteString
fromHex = do
  (res, err) <- B16.decode . toUTF8
  if BS.null err
    then return $ Just res
    else return Nothing

-- | Encodes a string to UTF8
toUTF8 :: String -> BS.ByteString
toUTF8 = toLazyByteString . stringUtf8

-- jl
