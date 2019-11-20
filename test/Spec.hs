{-

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

{-# LANGUAGE OverloadedStrings #-}

import Data.Char (chr, ord)
import Test.Hspec (Spec, context, describe, hspec, it, shouldBe)

import qualified Data.ByteString.Lazy as BS

import ARGTools

main :: IO ()
main = hspec $ describe "ARGTools" $ do
  fromHexSpec
  toUTF8Spec

fromHexSpec :: Spec
fromHexSpec = describe "fromHex" $ mapM_
  (\(label, input, expected) -> context label $
    it ("should be " ++ show expected) $
      fromHex input `shouldBe` expected)
  --  label,                      input,      expected
  [ ( "valid input (lower case)", "deadbeef", Just deadbeef )
  , ( "valid input (upper case)", "DEADBEEF", Just deadbeef )
  , ( "blank",                    "",         Just BS.empty )
  , ( "invalid input",            "xyzzy",    Nothing       )
  , ( "partially valid input",    "cabx",     Nothing       )
  ]
  where
    deadbeef = BS.pack [ 0xde, 0xad, 0xbe, 0xef ]

toUTF8Spec :: Spec
toUTF8Spec = describe "toUTF8" $ mapM_
  (\(label, input, expected) -> context label $
    it ("should be " ++ show expected) $
      toUTF8 input `shouldBe` expected)
  --  label,    input,              expected
  [ ( "ASCII",  "hello",            "hello"  )
  , ( "2-byte", chr 0xff : "x" ,    bs2      )
  , ( "3-byte", chr 0xffff : "x",   bs3      )
  , ( "4-byte", chr 0x10ffff : "x", bs4      )
  ]
  where
    bs2 = BS.pack [0xc3, 0xbf, x]
    bs3 = BS.pack [0xef, 0xbf, 0xbf, x]
    bs4 = BS.pack [0xf4, 0x8f, 0xbf, 0xbf, x]
    x = fromIntegral $ ord 'x'

-- jl
