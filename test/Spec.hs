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
import Data.Word (Word8)
import Test.Hspec (Spec, context, describe, hspec, it, shouldBe)

import qualified Data.ByteString.Lazy as BS

import ARGTools

main :: IO ()
main = hspec $ describe "ARGTools" $ do
  fromHexSpec
  fromUTF8Spec
  toUTF8Spec
  toBinarySpec
  toBitsSpec
  caesarSpec

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

fromUTF8Spec :: Spec
fromUTF8Spec = describe "fromUTF8" $ mapM_
  (\(label, input, expected) -> context label $
    it ("should be " ++ show expected) $
      fromUTF8 input `shouldBe` expected)
  --  label,     input,    expected
  [ ( "ASCII",   "hello",  Just "hello"              )
  , ( "2-byte",  bs2,      Just $ chr 0xff : "x"     )
  , ( "3-byte",  bs3,      Just $ chr 0xffff : "x"   )
  , ( "4-byte",  bs4,      Just $ chr 0x10ffff : "x" )
  , ( "invalid", "x\255x", Nothing                   )
  ]

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

toBinarySpec :: Spec
toBinarySpec = describe "toBinary" $ let
  input    = BS.pack [ 0xde, 0xad, 0xbe, 0xef ]
  expected =
    [ "11011110"
    , "10101101"
    , "10111110"
    , "11101111"
    ]

  in it ("should be " ++ show expected) $
    toBinary input `shouldBe` expected

toBitsSpec :: Spec
toBitsSpec = describe "toBits" $ mapM_
  (\(label, input, expected) -> context label $
    it ("should be " ++ expected) $
      toBits input `shouldBe` expected)
  --  label,          input, expected
  [ ( "leading zero", 0x7f,  "01111111" )
  , ( "leading one",  0xa1,  "10100001" )
  ]

caesarSpec :: Spec
caesarSpec = describe "caesar" $ mapM_
  (\(shift, input, expected) -> let
    label = "shift: " ++ show shift ++ ", input: " ++ input
    in context label $
      it ("should be " ++ expected) $
        caesar shift input `shouldBe` expected)
  --  shift, input,       expected
  [ ( 0,     "Foo, bar!", "Foo, bar!" )
  , ( 1,     "Abc, xyz.", "Bcd, yza." )
  , ( 1,     "ZZT",       "AAU"       )
  , ( -1,    "AbCdE!",    "ZaBcD!"    )
  ]

bs2 :: BS.ByteString
bs2 = BS.pack [0xc3, 0xbf, xChar]

bs3 :: BS.ByteString
bs3 = BS.pack [0xef, 0xbf, 0xbf, xChar]

bs4 :: BS.ByteString
bs4 = BS.pack [0xf4, 0x8f, 0xbf, 0xbf, xChar]

xChar :: Word8
xChar = fromIntegral $ ord 'x'

-- jl
