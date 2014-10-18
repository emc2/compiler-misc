-- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

module Tests.Text.Numbers(tests) where

import Data.Ratio
import Test.HUnitPlus.Base
import Text.Numbers

testHexDigit = [
    "0" ~: hexDigit '0' @?= 0x0,
    "1" ~: hexDigit '1' @?= 0x1,
    "2" ~: hexDigit '2' @?= 0x2,
    "3" ~: hexDigit '3' @?= 0x3,
    "4" ~: hexDigit '4' @?= 0x4,
    "5" ~: hexDigit '5' @?= 0x5,
    "6" ~: hexDigit '6' @?= 0x6,
    "7" ~: hexDigit '7' @?= 0x7,
    "8" ~: hexDigit '8' @?= 0x8,
    "9" ~: hexDigit '9' @?= 0x9,
    "a" ~: hexDigit 'a' @?= 0xa,
    "A" ~: hexDigit 'A' @?= 0xa,
    "b" ~: hexDigit 'b' @?= 0xb,
    "B" ~: hexDigit 'B' @?= 0xb,
    "c" ~: hexDigit 'c' @?= 0xc,
    "C" ~: hexDigit 'C' @?= 0xc,
    "d" ~: hexDigit 'd' @?= 0xd,
    "D" ~: hexDigit 'D' @?= 0xd,
    "e" ~: hexDigit 'e' @?= 0xe,
    "E" ~: hexDigit 'E' @?= 0xe,
    "f" ~: hexDigit 'f' @?= 0xf,
    "F" ~: hexDigit 'F' @?= 0xf
  ]

testDecDigit = [
    "0" ~: decDigit '0' @?= 0x0,
    "1" ~: decDigit '1' @?= 0x1,
    "2" ~: decDigit '2' @?= 0x2,
    "3" ~: decDigit '3' @?= 0x3,
    "4" ~: decDigit '4' @?= 0x4,
    "5" ~: decDigit '5' @?= 0x5,
    "6" ~: decDigit '6' @?= 0x6,
    "7" ~: decDigit '7' @?= 0x7,
    "8" ~: decDigit '8' @?= 0x8,
    "9" ~: decDigit '9' @?= 0x9
  ]

testOctDigit = [
    "0" ~: octDigit '0' @?= 0x0,
    "1" ~: octDigit '1' @?= 0x1,
    "2" ~: octDigit '2' @?= 0x2,
    "3" ~: octDigit '3' @?= 0x3,
    "4" ~: octDigit '4' @?= 0x4,
    "5" ~: octDigit '5' @?= 0x5,
    "6" ~: octDigit '6' @?= 0x6,
    "7" ~: octDigit '7' @?= 0x7
  ]

testBinDigit = [
    "0" ~: binDigit '0' @?= 0x0,
    "1" ~: binDigit '1' @?= 0x1
  ]

testHexNatural = [
    "0" ~: hexNatural "0" @?= 0x0,
    "1" ~: hexNatural "1" @?= 0x1,
    "2" ~: hexNatural "2" @?= 0x2,
    "3" ~: hexNatural "3" @?= 0x3,
    "4" ~: hexNatural "4" @?= 0x4,
    "5" ~: hexNatural "5" @?= 0x5,
    "6" ~: hexNatural "6" @?= 0x6,
    "7" ~: hexNatural "7" @?= 0x7,
    "8" ~: hexNatural "8" @?= 0x8,
    "9" ~: hexNatural "9" @?= 0x9,
    "a" ~: hexNatural "a" @?= 0xa,
    "A" ~: hexNatural "A" @?= 0xa,
    "b" ~: hexNatural "b" @?= 0xb,
    "B" ~: hexNatural "B" @?= 0xb,
    "c" ~: hexNatural "c" @?= 0xc,
    "C" ~: hexNatural "C" @?= 0xc,
    "d" ~: hexNatural "d" @?= 0xd,
    "D" ~: hexNatural "D" @?= 0xd,
    "e" ~: hexNatural "e" @?= 0xe,
    "E" ~: hexNatural "E" @?= 0xe,
    "f" ~: hexNatural "f" @?= 0xf,
    "F" ~: hexNatural "F" @?= 0xf,
    "10" ~: hexNatural "10" @?= 0x10,
    "11" ~: hexNatural "11" @?= 0x11,
    "12" ~: hexNatural "12" @?= 0x12,
    "13" ~: hexNatural "13" @?= 0x13,
    "14" ~: hexNatural "14" @?= 0x14,
    "15" ~: hexNatural "15" @?= 0x15,
    "16" ~: hexNatural "16" @?= 0x16,
    "17" ~: hexNatural "17" @?= 0x17,
    "18" ~: hexNatural "18" @?= 0x18,
    "19" ~: hexNatural "19" @?= 0x19,
    "1a" ~: hexNatural "1a" @?= 0x1a,
    "1A" ~: hexNatural "1A" @?= 0x1a,
    "1b" ~: hexNatural "1b" @?= 0x1b,
    "1B" ~: hexNatural "1B" @?= 0x1b,
    "1c" ~: hexNatural "1c" @?= 0x1c,
    "1C" ~: hexNatural "1C" @?= 0x1c,
    "1d" ~: hexNatural "1d" @?= 0x1d,
    "1D" ~: hexNatural "1D" @?= 0x1d,
    "1e" ~: hexNatural "1e" @?= 0x1e,
    "1E" ~: hexNatural "1E" @?= 0x1e,
    "1f" ~: hexNatural "1f" @?= 0x1f,
    "1F" ~: hexNatural "1F" @?= 0x1f,
    "01" ~: hexNatural "01" @?= 0x01,
    "11" ~: hexNatural "11" @?= 0x11,
    "21" ~: hexNatural "21" @?= 0x21,
    "31" ~: hexNatural "31" @?= 0x31,
    "41" ~: hexNatural "41" @?= 0x41,
    "51" ~: hexNatural "51" @?= 0x51,
    "61" ~: hexNatural "61" @?= 0x61,
    "71" ~: hexNatural "71" @?= 0x71,
    "81" ~: hexNatural "81" @?= 0x81,
    "91" ~: hexNatural "91" @?= 0x91,
    "a1" ~: hexNatural "a1" @?= 0xa1,
    "A1" ~: hexNatural "A1" @?= 0xa1,
    "b1" ~: hexNatural "b1" @?= 0xb1,
    "B1" ~: hexNatural "B1" @?= 0xb1,
    "c1" ~: hexNatural "c1" @?= 0xc1,
    "C1" ~: hexNatural "C1" @?= 0xc1,
    "d1" ~: hexNatural "d1" @?= 0xd1,
    "D1" ~: hexNatural "D1" @?= 0xd1,
    "e1" ~: hexNatural "e1" @?= 0xe1,
    "E1" ~: hexNatural "E1" @?= 0xe1,
    "f1" ~: hexNatural "f1" @?= 0xf1,
    "F1" ~: hexNatural "F1" @?= 0xf1,
    "111" ~: hexNatural "111" @?= 0x111,
    "2222" ~: hexNatural "2222" @?= 0x2222,
    "33333" ~: hexNatural "33333" @?= 0x33333,
    "444444" ~: hexNatural "444444" @?= 0x444444,
    "5555555" ~: hexNatural "5555555" @?= 0x5555555,
    "66666666" ~: hexNatural "66666666" @?= 0x66666666,
    "777777777" ~: hexNatural "777777777" @?= 0x777777777,
    "8888888888" ~: hexNatural "8888888888" @?= 0x8888888888,
    "99999999999" ~: hexNatural "99999999999" @?= 0x99999999999,
    "aaaaaaaaaaaa" ~: hexNatural "aaaaaaaaaaaa" @?= 0xaaaaaaaaaaaa,
    "bbbbbbbbbbbbb" ~: hexNatural "bbbbbbbbbbbbb" @?= 0xbbbbbbbbbbbbb,
    "cccccccccccccc" ~: hexNatural "cccccccccccccc" @?= 0xcccccccccccccc,
    "ddddddddddddddd" ~: hexNatural "ddddddddddddddd" @?= 0xddddddddddddddd,
    "eeeeeeeeeeeeeeee" ~: hexNatural "eeeeeeeeeeeeeeee" @?= 0xeeeeeeeeeeeeeeee,
    "fffffffffffffffff" ~: hexNatural "fffffffffffffffff" @?= 0xfffffffffffffffff
  ]

testDecNatural = [
    "0" ~: decNatural "0" @?= 0,
    "1" ~: decNatural "1" @?= 1,
    "2" ~: decNatural "2" @?= 2,
    "3" ~: decNatural "3" @?= 3,
    "4" ~: decNatural "4" @?= 4,
    "5" ~: decNatural "5" @?= 5,
    "6" ~: decNatural "6" @?= 6,
    "7" ~: decNatural "7" @?= 7,
    "8" ~: decNatural "8" @?= 8,
    "9" ~: decNatural "9" @?= 9,
    "10" ~: decNatural "10" @?= 10,
    "11" ~: decNatural "11" @?= 11,
    "12" ~: decNatural "12" @?= 12,
    "13" ~: decNatural "13" @?= 13,
    "14" ~: decNatural "14" @?= 14,
    "15" ~: decNatural "15" @?= 15,
    "16" ~: decNatural "16" @?= 16,
    "17" ~: decNatural "17" @?= 17,
    "18" ~: decNatural "18" @?= 18,
    "19" ~: decNatural "19" @?= 19,
    "01" ~: decNatural "01" @?= 01,
    "11" ~: decNatural "11" @?= 11,
    "21" ~: decNatural "21" @?= 21,
    "31" ~: decNatural "31" @?= 31,
    "41" ~: decNatural "41" @?= 41,
    "51" ~: decNatural "51" @?= 51,
    "61" ~: decNatural "61" @?= 61,
    "71" ~: decNatural "71" @?= 71,
    "81" ~: decNatural "81" @?= 81,
    "91" ~: decNatural "91" @?= 91,
    "111" ~: decNatural "111" @?= 111,
    "2222" ~: decNatural "2222" @?= 2222,
    "33333" ~: decNatural "33333" @?= 33333,
    "444444" ~: decNatural "444444" @?= 444444,
    "5555555" ~: decNatural "5555555" @?= 5555555,
    "66666666" ~: decNatural "66666666" @?= 66666666,
    "777777777" ~: decNatural "777777777" @?= 777777777,
    "8888888888" ~: decNatural "8888888888" @?= 8888888888,
    "99999999999" ~: decNatural "99999999999" @?= 99999999999
  ]

testOctNatural = [
    "0" ~: octNatural "0" @?= 0,
    "1" ~: octNatural "1" @?= 1,
    "2" ~: octNatural "2" @?= 2,
    "3" ~: octNatural "3" @?= 3,
    "4" ~: octNatural "4" @?= 4,
    "5" ~: octNatural "5" @?= 5,
    "6" ~: octNatural "6" @?= 6,
    "7" ~: octNatural "7" @?= 7,
    "10" ~: octNatural "10" @?= 8,
    "11" ~: octNatural "11" @?= 9,
    "12" ~: octNatural "12" @?= 10,
    "13" ~: octNatural "13" @?= 11,
    "14" ~: octNatural "14" @?= 12,
    "15" ~: octNatural "15" @?= 13,
    "16" ~: octNatural "16" @?= 14,
    "17" ~: octNatural "17" @?= 15,
    "01" ~: octNatural "01" @?= 1,
    "11" ~: octNatural "11" @?= 9,
    "21" ~: octNatural "21" @?= 17,
    "31" ~: octNatural "31" @?= 25,
    "41" ~: octNatural "41" @?= 33,
    "51" ~: octNatural "51" @?= 41,
    "61" ~: octNatural "61" @?= 49,
    "71" ~: octNatural "71" @?= 57,
    "111" ~: octNatural "111" @?= 73,
    "2222" ~: octNatural "2222" @?= 1170,
    "33333" ~: octNatural "33333" @?= 14043,
    "444444" ~: octNatural "444444" @?= 149796,
    "5555555" ~: octNatural "5555555" @?= 1497965,
    "66666666" ~: octNatural "66666666" @?= 14380470,
    "777777777" ~: octNatural "777777777" @?= 134217727
  ]

testBinNatural = [
    "0" ~: binNatural "0" @?= 0,
    "1" ~: binNatural "1" @?= 1,
    "10" ~: binNatural "10" @?= 2,
    "11" ~: binNatural "11" @?= 3,
    "01" ~: binNatural "01" @?= 1,
    "00" ~: binNatural "00" @?= 0,
    "111" ~: binNatural "111" @?= 0x7,
    "1010" ~: binNatural "1010" @?= 0xa,
    "101101" ~: binNatural "101101" @?= 0x2d,
    "11001110011" ~: binNatural "11001110011" @?= 0x673
  ]

testHexInteger = [
    "0" ~: hexInteger "0" @?= 0x0,
    "+0" ~: hexInteger "+0" @?= 0x0,
    "-0" ~: hexInteger "-0" @?= 0x0,
    "1" ~: hexInteger "1" @?= 0x1,
    "+1" ~: hexInteger "+1" @?= 0x1,
    "-1" ~: hexInteger "-1" @?= -0x1,
    "22" ~: hexInteger "22" @?= 0x22,
    "+22" ~: hexInteger "+22" @?= 0x22,
    "-22" ~: hexInteger "-22" @?= -0x22,
    "333" ~: hexInteger "333" @?= 0x333,
    "+333" ~: hexInteger "+333" @?= 0x333,
    "-333" ~: hexInteger "-333" @?= -0x333,
    "4444" ~: hexInteger "4444" @?= 0x4444,
    "+4444" ~: hexInteger "+4444" @?= 0x4444,
    "-4444" ~: hexInteger "-4444" @?= -0x4444,
    "55555" ~: hexInteger "55555" @?= 0x55555,
    "+55555" ~: hexInteger "+55555" @?= 0x55555,
    "-55555" ~: hexInteger "-55555" @?= -0x55555,
    "666666" ~: hexInteger "666666" @?= 0x666666,
    "+666666" ~: hexInteger "+666666" @?= 0x666666,
    "-666666" ~: hexInteger "-666666" @?= -0x666666,
    "7777777" ~: hexInteger "7777777" @?= 0x7777777,
    "+7777777" ~: hexInteger "+7777777" @?= 0x7777777,
    "-7777777" ~: hexInteger "-7777777" @?= -0x7777777,
    "88888888" ~: hexInteger "88888888" @?= 0x88888888,
    "+88888888" ~: hexInteger "+88888888" @?= 0x88888888,
    "-88888888" ~: hexInteger "-88888888" @?= -0x88888888,
    "999999999" ~: hexInteger "999999999" @?= 0x999999999,
    "+999999999" ~: hexInteger "+999999999" @?= 0x999999999,
    "-999999999" ~: hexInteger "-999999999" @?= -0x999999999,
    "aaaaaaaaaa" ~: hexInteger "aaaaaaaaaa" @?= 0xaaaaaaaaaa,
    "+aaaaaaaaaa" ~: hexInteger "+aaaaaaaaaa" @?= 0xaaaaaaaaaa,
    "-aaaaaaaaaa" ~: hexInteger "-aaaaaaaaaa" @?= -0xaaaaaaaaaa,
    "bbbbbbbbbbb" ~: hexInteger "bbbbbbbbbbb" @?= 0xbbbbbbbbbbb,
    "+bbbbbbbbbbb" ~: hexInteger "+bbbbbbbbbbb" @?= 0xbbbbbbbbbbb,
    "-bbbbbbbbbbb" ~: hexInteger "-bbbbbbbbbbb" @?= -0xbbbbbbbbbbb,
    "cccccccccccc" ~: hexInteger "cccccccccccc" @?= 0xcccccccccccc,
    "+cccccccccccc" ~: hexInteger "+cccccccccccc" @?= 0xcccccccccccc,
    "-cccccccccccc" ~: hexInteger "-cccccccccccc" @?= -0xcccccccccccc,
    "ddddddddddddd" ~: hexInteger "ddddddddddddd" @?= 0xddddddddddddd,
    "+ddddddddddddd" ~: hexInteger "+ddddddddddddd" @?= 0xddddddddddddd,
    "-ddddddddddddd" ~: hexInteger "-ddddddddddddd" @?= -0xddddddddddddd,
    "eeeeeeeeeeeeee" ~: hexInteger "eeeeeeeeeeeeee" @?= 0xeeeeeeeeeeeeee,
    "+eeeeeeeeeeeeee" ~: hexInteger "+eeeeeeeeeeeeee" @?= 0xeeeeeeeeeeeeee,
    "-eeeeeeeeeeeeee" ~: hexInteger "-eeeeeeeeeeeeee" @?= -0xeeeeeeeeeeeeee,
    "fffffffffffffff" ~: hexInteger "fffffffffffffff" @?= 0xfffffffffffffff,
    "+fffffffffffffff" ~: hexInteger "+fffffffffffffff" @?= 0xfffffffffffffff,
    "-fffffffffffffff" ~: hexInteger "-fffffffffffffff" @?= -0xfffffffffffffff
  ]

testDecInteger = [
    "0" ~: decInteger "0" @?= 0,
    "+0" ~: decInteger "+0" @?= 0,
    "-0" ~: decInteger "-0" @?= 0,
    "1" ~: decInteger "1" @?= 1,
    "+1" ~: decInteger "+1" @?= 1,
    "-1" ~: decInteger "-1" @?= -1,
    "22" ~: decInteger "22" @?= 22,
    "+22" ~: decInteger "+22" @?= 22,
    "-22" ~: decInteger "-22" @?= -22,
    "333" ~: decInteger "333" @?= 333,
    "+333" ~: decInteger "+333" @?= 333,
    "-333" ~: decInteger "-333" @?= -333,
    "4444" ~: decInteger "4444" @?= 4444,
    "+4444" ~: decInteger "+4444" @?= 4444,
    "-4444" ~: decInteger "-4444" @?= -4444,
    "55555" ~: decInteger "55555" @?= 55555,
    "+55555" ~: decInteger "+55555" @?= 55555,
    "-55555" ~: decInteger "-55555" @?= -55555,
    "666666" ~: decInteger "666666" @?= 666666,
    "+666666" ~: decInteger "+666666" @?= 666666,
    "-666666" ~: decInteger "-666666" @?= -666666,
    "7777777" ~: decInteger "7777777" @?= 7777777,
    "+7777777" ~: decInteger "+7777777" @?= 7777777,
    "-7777777" ~: decInteger "-7777777" @?= -7777777,
    "88888888" ~: decInteger "88888888" @?= 88888888,
    "+88888888" ~: decInteger "+88888888" @?= 88888888,
    "-88888888" ~: decInteger "-88888888" @?= -88888888,
    "999999999" ~: decInteger "999999999" @?= 999999999,
    "+999999999" ~: decInteger "+999999999" @?= 999999999,
    "-999999999" ~: decInteger "-999999999" @?= -999999999
  ]

testOctInteger = [
    "0" ~: octInteger "0" @?= 0,
    "+0" ~: octInteger "+0" @?= 0,
    "-0" ~: octInteger "-0" @?= 0,
    "1" ~: octInteger "1" @?= 1,
    "+1" ~: octInteger "+1" @?= 1,
    "-1" ~: octInteger "-1" @?= -1,
    "22" ~: octInteger "22" @?= 18,
    "+22" ~: octInteger "+22" @?= 18,
    "-22" ~: octInteger "-22" @?= -18,
    "333" ~: octInteger "333" @?= 219,
    "+333" ~: octInteger "+333" @?= 219,
    "-333" ~: octInteger "-333" @?= -219,
    "4444" ~: octInteger "4444" @?= 2340,
    "+4444" ~: octInteger "+4444" @?= 2340,
    "-4444" ~: octInteger "-4444" @?= -2340,
    "55555" ~: octInteger "55555" @?= 23405,
    "+55555" ~: octInteger "+55555" @?= 23405,
    "-55555" ~: octInteger "-55555" @?= -23405,
    "666666" ~: octInteger "666666" @?= 224694,
    "+666666" ~: octInteger "+666666" @?= 224694,
    "-666666" ~: octInteger "-666666" @?= -224694,
    "7777777" ~: octInteger "7777777" @?= 2097151,
    "+7777777" ~: octInteger "+7777777" @?= 2097151,
    "-7777777" ~: octInteger "-7777777" @?= -2097151
  ]

testBinInteger = [
    "0" ~: binInteger "0" @?= 0,
    "+0" ~: binInteger "+0" @?= 0,
    "-0" ~: binInteger "-0" @?= 0,
    "1" ~: binInteger "1" @?= 1,
    "+1" ~: binInteger "+1" @?= 1,
    "-1" ~: binInteger "-1" @?= -1,
    "1010" ~: binInteger "1010" @?= 0xa,
    "+1010" ~: binInteger "+1010" @?= 0xa,
    "-1010" ~: binInteger "-1010" @?= -0xa,
    "111111" ~: binInteger "111111" @?= 0x3f,
    "+111111" ~: binInteger "+111111" @?= 0x3f,
    "-111111" ~: binInteger "-111111" @?= -0x3f,
    "100100100100" ~: binInteger "100100100100" @?= 0x924,
    "+100100100100" ~: binInteger "+100100100100" @?= 0x924,
    "-100100100100" ~: binInteger "-100100100100" @?= -0x924
  ]

testHexRational = [
    "0" ~: hexRational "0" @?= 0x0,
    "+0" ~: hexRational "+0" @?= 0x0,
    "-0" ~: hexRational "-0" @?= 0x0,
    "1" ~: hexRational "1" @?= 0x1,
    "+1" ~: hexRational "+1" @?= 0x1,
    "-1" ~: hexRational "-1" @?= -0x1,
    "22" ~: hexRational "22" @?= 0x22,
    "+22" ~: hexRational "+22" @?= 0x22,
    "-22" ~: hexRational "-22" @?= -0x22,
    "333" ~: hexRational "333" @?= 0x333,
    "+333" ~: hexRational "+333" @?= 0x333,
    "-333" ~: hexRational "-333" @?= -0x333,
    "4444" ~: hexRational "4444" @?= 0x4444,
    "+4444" ~: hexRational "+4444" @?= 0x4444,
    "-4444" ~: hexRational "-4444" @?= -0x4444,
    "55555" ~: hexRational "55555" @?= 0x55555,
    "+55555" ~: hexRational "+55555" @?= 0x55555,
    "-55555" ~: hexRational "-55555" @?= -0x55555,
    "666666" ~: hexRational "666666" @?= 0x666666,
    "+666666" ~: hexRational "+666666" @?= 0x666666,
    "-666666" ~: hexRational "-666666" @?= -0x666666,
    "7777777" ~: hexRational "7777777" @?= 0x7777777,
    "+7777777" ~: hexRational "+7777777" @?= 0x7777777,
    "-7777777" ~: hexRational "-7777777" @?= -0x7777777,
    "88888888" ~: hexRational "88888888" @?= 0x88888888,
    "+88888888" ~: hexRational "+88888888" @?= 0x88888888,
    "-88888888" ~: hexRational "-88888888" @?= -0x88888888,
    "999999999" ~: hexRational "999999999" @?= 0x999999999,
    "+999999999" ~: hexRational "+999999999" @?= 0x999999999,
    "-999999999" ~: hexRational "-999999999" @?= -0x999999999,
    "aaaaaaaaaa" ~: hexRational "aaaaaaaaaa" @?= 0xaaaaaaaaaa,
    "+aaaaaaaaaa" ~: hexRational "+aaaaaaaaaa" @?= 0xaaaaaaaaaa,
    "-aaaaaaaaaa" ~: hexRational "-aaaaaaaaaa" @?= -0xaaaaaaaaaa,
    "bbbbbbbbbbb" ~: hexRational "bbbbbbbbbbb" @?= 0xbbbbbbbbbbb,
    "+bbbbbbbbbbb" ~: hexRational "+bbbbbbbbbbb" @?= 0xbbbbbbbbbbb,
    "-bbbbbbbbbbb" ~: hexRational "-bbbbbbbbbbb" @?= -0xbbbbbbbbbbb,
    "cccccccccccc" ~: hexRational "cccccccccccc" @?= 0xcccccccccccc,
    "+cccccccccccc" ~: hexRational "+cccccccccccc" @?= 0xcccccccccccc,
    "-cccccccccccc" ~: hexRational "-cccccccccccc" @?= -0xcccccccccccc,
    "ddddddddddddd" ~: hexRational "ddddddddddddd" @?= 0xddddddddddddd,
    "+ddddddddddddd" ~: hexRational "+ddddddddddddd" @?= 0xddddddddddddd,
    "-ddddddddddddd" ~: hexRational "-ddddddddddddd" @?= -0xddddddddddddd,
    "eeeeeeeeeeeeee" ~: hexRational "eeeeeeeeeeeeee" @?= 0xeeeeeeeeeeeeee,
    "+eeeeeeeeeeeeee" ~: hexRational "+eeeeeeeeeeeeee" @?= 0xeeeeeeeeeeeeee,
    "-eeeeeeeeeeeeee" ~: hexRational "-eeeeeeeeeeeeee" @?= -0xeeeeeeeeeeeeee,
    "fffffffffffffff" ~: hexRational "fffffffffffffff" @?= 0xfffffffffffffff,
    "+fffffffffffffff" ~: hexRational "+fffffffffffffff" @?= 0xfffffffffffffff,
    "-fffffffffffffff" ~: hexRational "-fffffffffffffff" @?= -0xfffffffffffffff,
    "0.0" ~: hexRational "0.0" @?=
             0x0,
    "+0.0" ~: hexRational "+0.0" @?=
              0x0,
    "-0.0" ~: hexRational "-0.0" @?=
              0x0,
    "0.1" ~: hexRational "0.1" @?=
             0x1 % 0x10,
    "+0.1" ~: hexRational "+0.1" @?=
              0x1 % 0x10,
    "-0.1" ~: hexRational "-0.1" @?=
              -(0x1 % 0x10),
    "0.22" ~: hexRational "0.22" @?=
              0x22 % 0x100,
    "+0.22" ~: hexRational "+0.22" @?=
               0x22 % 0x100,
    "-0.22" ~: hexRational "-0.22" @?=
               -(0x22 % 0x100),
    "0.333" ~: hexRational "0.333" @?=
               0x333 % 0x1000,
    "+0.333" ~: hexRational "+0.333" @?=
                0x333 % 0x1000,
    "-0.333" ~: hexRational "-0.333" @?=
                -0x333 % 0x1000,
    "0.4444" ~: hexRational "0.4444" @?=
                0x4444 % 0x10000,
    "+0.4444" ~: hexRational "+0.4444" @?=
                 0x4444 % 0x10000,
    "-0.4444" ~: hexRational "-0.4444" @?=
                 -0x4444 % 0x10000,
    "0.55555" ~: hexRational "0.55555" @?=
                 0x55555 % 0x100000,
    "+0.55555" ~: hexRational "+0.55555" @?=
                  0x55555 % 0x100000,
    "-0.55555" ~: hexRational "-0.55555" @?=
                  -(0x55555 % 0x100000),
    "0.666666" ~: hexRational "0.666666" @?=
                  0x666666 % 0x1000000,
    "+0.666666" ~: hexRational "+0.666666" @?=
                   0x666666 % 0x1000000,
    "-0.666666" ~: hexRational "-0.666666" @?=
                   -(0x666666 % 0x1000000),
    "0.7777777" ~: hexRational "0.7777777" @?=
                   0x7777777 % 0x10000000,
    "+0.7777777" ~: hexRational "+0.7777777" @?=
                    0x7777777 % 0x10000000,
    "-0.7777777" ~: hexRational "-0.7777777" @?=
                    -(0x7777777 % 0x10000000),
    "0.88888888" ~: hexRational "0.88888888" @?=
                    0x88888888 % 0x100000000,
    "+0.88888888" ~: hexRational "+0.88888888" @?=
                     0x88888888 % 0x100000000,
    "-0.88888888" ~: hexRational "-0.88888888" @?=
                     -(0x88888888 % 0x100000000),
    "0.999999999" ~: hexRational "0.999999999" @?=
                     0x999999999 % 0x1000000000,
    "+0.999999999" ~: hexRational "+0.999999999" @?=
                     0x999999999 % 0x1000000000,
    "-0.999999999" ~: hexRational "-0.999999999" @?=
                     -(0x999999999 % 0x1000000000),
    "0.aaaaaaaaaa" ~: hexRational "0.aaaaaaaaaa" @?=
                      0xaaaaaaaaaa % 0x10000000000,
    "+0.aaaaaaaaaa" ~: hexRational "+0.aaaaaaaaaa" @?=
                       0xaaaaaaaaaa % 0x10000000000,
    "-0.aaaaaaaaaa" ~: hexRational "-0.aaaaaaaaaa" @?=
                       -(0xaaaaaaaaaa % 0x10000000000),
    "0.bbbbbbbbbbb" ~: hexRational "0.bbbbbbbbbbb" @?=
                       0xbbbbbbbbbbb % 0x100000000000,
    "+0.bbbbbbbbbbb" ~: hexRational "+0.bbbbbbbbbbb" @?=
                        0xbbbbbbbbbbb % 0x100000000000,
    "-0.bbbbbbbbbbb" ~: hexRational "-0.bbbbbbbbbbb" @?=
                        -(0xbbbbbbbbbbb % 0x100000000000),
    "0.cccccccccccc" ~: hexRational "0.cccccccccccc" @?=
                        0xcccccccccccc % 0x1000000000000,
    "+0.cccccccccccc" ~: hexRational "+0.cccccccccccc" @?=
                         0xcccccccccccc % 0x1000000000000,
    "-0.cccccccccccc" ~: hexRational "-0.cccccccccccc" @?=
                         -(0xcccccccccccc % 0x1000000000000),
    "0.ddddddddddddd" ~: hexRational "0.ddddddddddddd" @?=
                         0xddddddddddddd % 0x10000000000000,
    "+0.ddddddddddddd" ~: hexRational "+0.ddddddddddddd" @?=
                          0xddddddddddddd % 0x10000000000000,
    "-0.ddddddddddddd" ~: hexRational "-0.ddddddddddddd" @?=
                          -(0xddddddddddddd % 0x10000000000000),
    "0.eeeeeeeeeeeeee" ~: hexRational "0.eeeeeeeeeeeeee" @?=
                          0xeeeeeeeeeeeeee % 0x100000000000000,
    "+0.eeeeeeeeeeeeee" ~: hexRational "+0.eeeeeeeeeeeeee" @?=
                           0xeeeeeeeeeeeeee % 0x100000000000000,
    "-0.eeeeeeeeeeeeee" ~: hexRational "-0.eeeeeeeeeeeeee" @?=
                           -(0xeeeeeeeeeeeeee % 0x100000000000000),
    "0.fffffffffffffff" ~: hexRational "0.fffffffffffffff" @?=
                           0xfffffffffffffff % 0x1000000000000000,
    "+0.fffffffffffffff" ~: hexRational "+0.fffffffffffffff" @?=
                            0xfffffffffffffff % 0x1000000000000000,
    "-0.fffffffffffffff" ~: hexRational "-0.fffffffffffffff" @?=
                            -(0xfffffffffffffff % 0x1000000000000000),
    "1.2" ~: hexRational "1.2" @?=
             0x12 % 0x10,
    "+1.2" ~: hexRational "+1.2" @?=
              0x12 % 0x10,
    "-1.2" ~: hexRational "-1.2" @?=
              -(0x12 % 0x10),
    "12.34" ~: hexRational "12.34" @?=
              0x1234 % 0x100,
    "+12.34" ~: hexRational "+12.34" @?=
               0x1234 % 0x100,
    "-12.34" ~: hexRational "-12.34" @?=
               -(0x1234 % 0x100),
    "123.456" ~: hexRational "123.456" @?=
               0x123456 % 0x1000,
    "+123.456" ~: hexRational "+123.456" @?=
                0x123456 % 0x1000,
    "-123.456" ~: hexRational "-123.456" @?=
                -0x123456 % 0x1000,
    "1234.5678" ~: hexRational "1234.5678" @?=
                0x12345678 % 0x10000,
    "+1234.5678" ~: hexRational "+1234.5678" @?=
                 0x12345678 % 0x10000,
    "-1234.5678" ~: hexRational "-1234.5678" @?=
                 -0x12345678 % 0x10000,
    "12345.6789a" ~: hexRational "12345.6789a" @?=
                 0x123456789a % 0x100000,
    "+12345.6789a" ~: hexRational "+12345.6789a" @?=
                  0x123456789a % 0x100000,
    "-12345.6789a" ~: hexRational "-12345.6789a" @?=
                  -(0x123456789a % 0x100000),
    "123456.789abc" ~: hexRational "123456.789abc" @?=
                  0x123456789abc % 0x1000000,
    "+123456.789abc" ~: hexRational "+123456.789abc" @?=
                   0x123456789abc % 0x1000000,
    "-123456.789abc" ~: hexRational "-123456.789abc" @?=
                   -(0x123456789abc % 0x1000000),
    "1234567.89abcde" ~: hexRational "1234567.89abcde" @?=
                   0x123456789abcde % 0x10000000,
    "+1234567.89abcde" ~: hexRational "+1234567.89abcde" @?=
                    0x123456789abcde % 0x10000000,
    "-1234567.89abcde" ~: hexRational "-1234567.89abcde" @?=
                    -(0x123456789abcde % 0x10000000),
    "1234567.89abcdef" ~: hexRational "1234567.89abcdef" @?=
                   0x123456789abcdef % 0x100000000,
    "+1234567.89abcdef" ~: hexRational "+1234567.89abcdef" @?=
                    0x123456789abcdef % 0x100000000,
    "-1234567.89abcdef" ~: hexRational "-1234567.89abcdef" @?=
                    -(0x123456789abcdef % 0x100000000)
  ]

testDecRational = [
    "0" ~: decRational "0" @?= 0,
    "+0" ~: decRational "+0" @?= 0,
    "-0" ~: decRational "-0" @?= 0,
    "1" ~: decRational "1" @?= 1,
    "+1" ~: decRational "+1" @?= 1,
    "-1" ~: decRational "-1" @?= -1,
    "22" ~: decRational "22" @?= 22,
    "+22" ~: decRational "+22" @?= 22,
    "-22" ~: decRational "-22" @?= -22,
    "333" ~: decRational "333" @?= 333,
    "+333" ~: decRational "+333" @?= 333,
    "-333" ~: decRational "-333" @?= -333,
    "4444" ~: decRational "4444" @?= 4444,
    "+4444" ~: decRational "+4444" @?= 4444,
    "-4444" ~: decRational "-4444" @?= -4444,
    "55555" ~: decRational "55555" @?= 55555,
    "+55555" ~: decRational "+55555" @?= 55555,
    "-55555" ~: decRational "-55555" @?= -55555,
    "666666" ~: decRational "666666" @?= 666666,
    "+666666" ~: decRational "+666666" @?= 666666,
    "-666666" ~: decRational "-666666" @?= -666666,
    "7777777" ~: decRational "7777777" @?= 7777777,
    "+7777777" ~: decRational "+7777777" @?= 7777777,
    "-7777777" ~: decRational "-7777777" @?= -7777777,
    "88888888" ~: decRational "88888888" @?= 88888888,
    "+88888888" ~: decRational "+88888888" @?= 88888888,
    "-88888888" ~: decRational "-88888888" @?= -88888888,
    "999999999" ~: decRational "999999999" @?= 999999999,
    "+999999999" ~: decRational "+999999999" @?= 999999999,
    "-999999999" ~: decRational "-999999999" @?= -999999999,
    "0.0" ~: decRational "0.0" @?=
             0,
    "+0.0" ~: decRational "+0.0" @?=
              0,
    "-0.0" ~: decRational "-0.0" @?=
              0,
    "0.1" ~: decRational "0.1" @?=
             1 % 10,
    "+0.1" ~: decRational "+0.1" @?=
              1 % 10,
    "-0.1" ~: decRational "-0.1" @?=
              -(1 % 10),
    "0.22" ~: decRational "0.22" @?=
              22 % 100,
    "+0.22" ~: decRational "+0.22" @?=
               22 % 100,
    "-0.22" ~: decRational "-0.22" @?=
               -(22 % 100),
    "0.333" ~: decRational "0.333" @?=
               333 % 1000,
    "+0.333" ~: decRational "+0.333" @?=
                333 % 1000,
    "-0.333" ~: decRational "-0.333" @?=
                -333 % 1000,
    "0.4444" ~: decRational "0.4444" @?=
                4444 % 10000,
    "+0.4444" ~: decRational "+0.4444" @?=
                 4444 % 10000,
    "-0.4444" ~: decRational "-0.4444" @?=
                 -4444 % 10000,
    "0.55555" ~: decRational "0.55555" @?=
                 55555 % 100000,
    "+0.55555" ~: decRational "+0.55555" @?=
                  55555 % 100000,
    "-0.55555" ~: decRational "-0.55555" @?=
                  -(55555 % 100000),
    "0.666666" ~: decRational "0.666666" @?=
                  666666 % 1000000,
    "+0.666666" ~: decRational "+0.666666" @?=
                   666666 % 1000000,
    "-0.666666" ~: decRational "-0.666666" @?=
                   -(666666 % 1000000),
    "0.7777777" ~: decRational "0.7777777" @?=
                   7777777 % 10000000,
    "+0.7777777" ~: decRational "+0.7777777" @?=
                    7777777 % 10000000,
    "-0.7777777" ~: decRational "-0.7777777" @?=
                    -(7777777 % 10000000),
    "0.88888888" ~: decRational "0.88888888" @?=
                    88888888 % 100000000,
    "+0.88888888" ~: decRational "+0.88888888" @?=
                     88888888 % 100000000,
    "-0.88888888" ~: decRational "-0.88888888" @?=
                     -(88888888 % 100000000),
    "0.999999999" ~: decRational "0.999999999" @?=
                     999999999 % 1000000000,
    "+0.999999999" ~: decRational "+0.999999999" @?=
                     999999999 % 1000000000,
    "-0.999999999" ~: decRational "-0.999999999" @?=
                     -(999999999 % 1000000000),
    "1.2" ~: decRational "1.2" @?=
             12 % 10,
    "+1.2" ~: decRational "+1.2" @?=
              12 % 10,
    "-1.2" ~: decRational "-1.2" @?=
              -(12 % 10),
    "12.34" ~: decRational "12.34" @?=
              1234 % 100,
    "+12.34" ~: decRational "+12.34" @?=
               1234 % 100,
    "-12.34" ~: decRational "-12.34" @?=
               -(1234 % 100),
    "123.456" ~: decRational "123.456" @?=
               123456 % 1000,
    "+123.456" ~: decRational "+123.456" @?=
                123456 % 1000,
    "-123.456" ~: decRational "-123.456" @?=
                -123456 % 1000,
    "1234.5678" ~: decRational "1234.5678" @?=
                12345678 % 10000,
    "+1234.5678" ~: decRational "+1234.5678" @?=
                 12345678 % 10000,
    "-1234.5678" ~: decRational "-1234.5678" @?=
                 -12345678 % 10000,
    "12345.6789" ~: decRational "12345.6789" @?=
                 123456789 % 10000,
    "+12345.6789" ~: decRational "+12345.6789" @?=
                  123456789 % 10000,
    "-12345.6789" ~: decRational "-12345.6789" @?=
                  -(123456789 % 10000)
  ]

testOctRational = [
    "0" ~: octRational "0" @?= 0,
    "+0" ~: octRational "+0" @?= 0,
    "-0" ~: octRational "-0" @?= 0,
    "1" ~: octRational "1" @?= 1,
    "+1" ~: octRational "+1" @?= 1,
    "-1" ~: octRational "-1" @?= -1,
    "22" ~: octRational "22" @?= 18,
    "+22" ~: octRational "+22" @?= 18,
    "-22" ~: octRational "-22" @?= -18,
    "333" ~: octRational "333" @?= 219,
    "+333" ~: octRational "+333" @?= 219,
    "-333" ~: octRational "-333" @?= -219,
    "4444" ~: octRational "4444" @?= 2340,
    "+4444" ~: octRational "+4444" @?= 2340,
    "-4444" ~: octRational "-4444" @?= -2340,
    "55555" ~: octRational "55555" @?= 23405,
    "+55555" ~: octRational "+55555" @?= 23405,
    "-55555" ~: octRational "-55555" @?= -23405,
    "666666" ~: octRational "666666" @?= 224694,
    "+666666" ~: octRational "+666666" @?= 224694,
    "-666666" ~: octRational "-666666" @?= -224694,
    "7777777" ~: octRational "7777777" @?= 2097151,
    "+7777777" ~: octRational "+7777777" @?= 2097151,
    "-7777777" ~: octRational "-7777777" @?= -2097151,
    "0.0" ~: octRational "0.0" @?=
             0,
    "+0.0" ~: octRational "+0.0" @?=
              0,
    "-0.0" ~: octRational "-0.0" @?=
              0,
    "0.1" ~: octRational "0.1" @?=
             1 % 8,
    "+0.1" ~: octRational "+0.1" @?=
              1 % 8,
    "-0.1" ~: octRational "-0.1" @?=
              -(1 % 8),
    "0.22" ~: octRational "0.22" @?=
              18 % 64,
    "+0.22" ~: octRational "+0.22" @?=
               18 % 64,
    "-0.22" ~: octRational "-0.22" @?=
               -(18 % 64),
    "0.333" ~: octRational "0.333" @?=
               219 % 512,
    "+0.333" ~: octRational "+0.333" @?=
                219 % 512,
    "-0.333" ~: octRational "-0.333" @?=
                -219 % 512,
    "0.4444" ~: octRational "0.4444" @?=
                2340 % 4096,
    "+0.4444" ~: octRational "+0.4444" @?=
                 2340 % 4096,
    "-0.4444" ~: octRational "-0.4444" @?=
                 -2340 % 4096,
    "0.55555" ~: octRational "0.55555" @?=
                 23405 % 32768,
    "+0.55555" ~: octRational "+0.55555" @?=
                  23405 % 32768,
    "-0.55555" ~: octRational "-0.55555" @?=
                  -(23405 % 32768),
    "0.666666" ~: octRational "0.666666" @?=
                  224694 % 262144,
    "+0.666666" ~: octRational "+0.666666" @?=
                   224694 % 262144,
    "-0.666666" ~: octRational "-0.666666" @?=
                   -(224694 % 262144),
    "0.7777777" ~: octRational "0.7777777" @?=
                   2097151 % 2097152,
    "+0.7777777" ~: octRational "+0.7777777" @?=
                    2097151 % 2097152,
    "-0.7777777" ~: octRational "-0.7777777" @?=
                    -(2097151 % 2097152),
    "1.2" ~: octRational "1.2" @?=
             10 % 8,
    "+1.2" ~: octRational "+1.2" @?=
              10 % 8,
    "-1.2" ~: octRational "-1.2" @?=
              -(10 % 8),
    "12.34" ~: octRational "12.34" @?=
              668 % 64,
    "+12.34" ~: octRational "+12.34" @?=
               668 % 64,
    "-12.34" ~: octRational "-12.34" @?=
               -(668 % 64),
    "123.456" ~: octRational "123.456" @?=
               42798 % 512,
    "+123.456" ~: octRational "+123.456" @?=
                42798 % 512,
    "-123.456" ~: octRational "-123.456" @?=
                -42798 % 512,
    "1234.567" ~: octRational "1234.567" @?=
                342391 % 512,
    "+1234.567" ~: octRational "+1234.567" @?=
                 342391 % 512,
    "-1234.567" ~: octRational "-1234.567" @?=
                 -342391 % 512
  ]

testBinRational = [
    "0" ~: binRational "0" @?= 0,
    "+0" ~: binRational "+0" @?= 0,
    "-0" ~: binRational "-0" @?= 0,
    "1" ~: binRational "1" @?= 1,
    "+1" ~: binRational "+1" @?= 1,
    "-1" ~: binRational "-1" @?= -1,
    "1010" ~: binRational "1010" @?= 0xa,
    "+1010" ~: binRational "+1010" @?= 0xa,
    "-1010" ~: binRational "-1010" @?= -0xa,
    "111111" ~: binRational "111111" @?= 0x3f,
    "+111111" ~: binRational "+111111" @?= 0x3f,
    "-111111" ~: binRational "-111111" @?= -0x3f,
    "100100100100" ~: binRational "100100100100" @?= 0x924,
    "+100100100100" ~: binRational "+100100100100" @?= 0x924,
    "-100100100100" ~: binRational "-100100100100" @?= -0x924,
    "0.0" ~: binRational "0.0" @?= 0,
    "+0.0" ~: binRational "+0.0" @?= 0,
    "-0.0" ~: binRational "-0.0" @?= 0,
    "0.1" ~: binRational "0.1" @?= 1 % 0x2,
    "+0.1" ~: binRational "+0.1" @?= 1 % 0x2,
    "-0.1" ~: binRational "-0.1" @?= -1 % 0x2,
    "0.1010" ~: binRational "0.1010" @?= 0xa % 0x10,
    "+0.1010" ~: binRational "+0.1010" @?= 0xa % 0x10,
    "-0.1010" ~: binRational "-0.1010" @?= -0xa % 0x10,
    "0.111111" ~: binRational "0.111111" @?= 0x3f % 0x40,
    "+0.111111" ~: binRational "+0.111111" @?= 0x3f % 0x40,
    "-0.111111" ~: binRational "-0.111111" @?= -0x3f % 0x40,
    "0.100100100100" ~: binRational "0.100100100100" @?= 0x924 % 0x1000,
    "+0.100100100100" ~: binRational "+0.100100100100" @?= 0x924 % 0x1000,
    "-0.100100100100" ~: binRational "-0.100100100100" @?= -0x924 % 0x1000,
    "1.1" ~: binRational "1.1" @?= 0x3 % 0x2,
    "+1.1" ~: binRational "+1.1" @?= 0x3 % 0x2,
    "-1.1" ~: binRational "-1.1" @?= -0x3 % 0x2,
    "1101.1010" ~: binRational "1101.1010" @?= 0xda % 0x10,
    "+1101.1010" ~: binRational "+1101.1010" @?= 0xda % 0x10,
    "-1101.1010" ~: binRational "-1101.1010" @?= -0xda % 0x10,
    "11010.111111" ~: binRational "11010.111111" @?= 0x6bf % 0x40,
    "+11010.111111" ~: binRational "+11010.111111" @?= 0x6bf % 0x40,
    "-11010.111111" ~: binRational "-11010.111111" @?= -0x6bf % 0x40,
    "110010100110.100100100100" ~: binRational "110010100110.100100100100" @?=
                                   0xca6924 % 0x1000,
    "+110010100110.100100100100" ~: binRational "+110010100110.100100100100" @?=
                                    0xca6924 % 0x1000,
    "-110010100110.100100100100" ~: binRational "-110010100110.100100100100" @?=
                                    -0xca6924 % 0x1000
  ]

testHexLiteral = [
    "0" ~: hexLiteral "0" @?= 0x0,
    "+0" ~: hexLiteral "+0" @?= 0x0,
    "-0" ~: hexLiteral "-0" @?= 0x0,
    "1" ~: hexLiteral "1" @?= 0x1,
    "+1" ~: hexLiteral "+1" @?= 0x1,
    "-1" ~: hexLiteral "-1" @?= -0x1,
    "22" ~: hexLiteral "22" @?= 0x22,
    "+22" ~: hexLiteral "+22" @?= 0x22,
    "-22" ~: hexLiteral "-22" @?= -0x22,
    "333" ~: hexLiteral "333" @?= 0x333,
    "+333" ~: hexLiteral "+333" @?= 0x333,
    "-333" ~: hexLiteral "-333" @?= -0x333,
    "4444" ~: hexLiteral "4444" @?= 0x4444,
    "+4444" ~: hexLiteral "+4444" @?= 0x4444,
    "-4444" ~: hexLiteral "-4444" @?= -0x4444,
    "55555" ~: hexLiteral "55555" @?= 0x55555,
    "+55555" ~: hexLiteral "+55555" @?= 0x55555,
    "-55555" ~: hexLiteral "-55555" @?= -0x55555,
    "666666" ~: hexLiteral "666666" @?= 0x666666,
    "+666666" ~: hexLiteral "+666666" @?= 0x666666,
    "-666666" ~: hexLiteral "-666666" @?= -0x666666,
    "7777777" ~: hexLiteral "7777777" @?= 0x7777777,
    "+7777777" ~: hexLiteral "+7777777" @?= 0x7777777,
    "-7777777" ~: hexLiteral "-7777777" @?= -0x7777777,
    "88888888" ~: hexLiteral "88888888" @?= 0x88888888,
    "+88888888" ~: hexLiteral "+88888888" @?= 0x88888888,
    "-88888888" ~: hexLiteral "-88888888" @?= -0x88888888,
    "999999999" ~: hexLiteral "999999999" @?= 0x999999999,
    "+999999999" ~: hexLiteral "+999999999" @?= 0x999999999,
    "-999999999" ~: hexLiteral "-999999999" @?= -0x999999999,
    "aaaaaaaaaa" ~: hexLiteral "aaaaaaaaaa" @?= 0xaaaaaaaaaa,
    "+aaaaaaaaaa" ~: hexLiteral "+aaaaaaaaaa" @?= 0xaaaaaaaaaa,
    "-aaaaaaaaaa" ~: hexLiteral "-aaaaaaaaaa" @?= -0xaaaaaaaaaa,
    "bbbbbbbbbbb" ~: hexLiteral "bbbbbbbbbbb" @?= 0xbbbbbbbbbbb,
    "+bbbbbbbbbbb" ~: hexLiteral "+bbbbbbbbbbb" @?= 0xbbbbbbbbbbb,
    "-bbbbbbbbbbb" ~: hexLiteral "-bbbbbbbbbbb" @?= -0xbbbbbbbbbbb,
    "cccccccccccc" ~: hexLiteral "cccccccccccc" @?= 0xcccccccccccc,
    "+cccccccccccc" ~: hexLiteral "+cccccccccccc" @?= 0xcccccccccccc,
    "-cccccccccccc" ~: hexLiteral "-cccccccccccc" @?= -0xcccccccccccc,
    "ddddddddddddd" ~: hexLiteral "ddddddddddddd" @?= 0xddddddddddddd,
    "+ddddddddddddd" ~: hexLiteral "+ddddddddddddd" @?= 0xddddddddddddd,
    "-ddddddddddddd" ~: hexLiteral "-ddddddddddddd" @?= -0xddddddddddddd,
    "eeeeeeeeeeeeee" ~: hexLiteral "eeeeeeeeeeeeee" @?= 0xeeeeeeeeeeeeee,
    "+eeeeeeeeeeeeee" ~: hexLiteral "+eeeeeeeeeeeeee" @?= 0xeeeeeeeeeeeeee,
    "-eeeeeeeeeeeeee" ~: hexLiteral "-eeeeeeeeeeeeee" @?= -0xeeeeeeeeeeeeee,
    "fffffffffffffff" ~: hexLiteral "fffffffffffffff" @?= 0xfffffffffffffff,
    "+fffffffffffffff" ~: hexLiteral "+fffffffffffffff" @?= 0xfffffffffffffff,
    "-fffffffffffffff" ~: hexLiteral "-fffffffffffffff" @?= -0xfffffffffffffff,
    "0.0" ~: hexLiteral "0.0" @?=
             0x0,
    "+0.0" ~: hexLiteral "+0.0" @?=
              0x0,
    "-0.0" ~: hexLiteral "-0.0" @?=
              0x0,
    "0.1" ~: hexLiteral "0.1" @?=
             0x1 % 0x10,
    "+0.1" ~: hexLiteral "+0.1" @?=
              0x1 % 0x10,
    "-0.1" ~: hexLiteral "-0.1" @?=
              -(0x1 % 0x10),
    "0.22" ~: hexLiteral "0.22" @?=
              0x22 % 0x100,
    "+0.22" ~: hexLiteral "+0.22" @?=
               0x22 % 0x100,
    "-0.22" ~: hexLiteral "-0.22" @?=
               -(0x22 % 0x100),
    "0.333" ~: hexLiteral "0.333" @?=
               0x333 % 0x1000,
    "+0.333" ~: hexLiteral "+0.333" @?=
                0x333 % 0x1000,
    "-0.333" ~: hexLiteral "-0.333" @?=
                -0x333 % 0x1000,
    "0.4444" ~: hexLiteral "0.4444" @?=
                0x4444 % 0x10000,
    "+0.4444" ~: hexLiteral "+0.4444" @?=
                 0x4444 % 0x10000,
    "-0.4444" ~: hexLiteral "-0.4444" @?=
                 -0x4444 % 0x10000,
    "0.55555" ~: hexLiteral "0.55555" @?=
                 0x55555 % 0x100000,
    "+0.55555" ~: hexLiteral "+0.55555" @?=
                  0x55555 % 0x100000,
    "-0.55555" ~: hexLiteral "-0.55555" @?=
                  -(0x55555 % 0x100000),
    "0.666666" ~: hexLiteral "0.666666" @?=
                  0x666666 % 0x1000000,
    "+0.666666" ~: hexLiteral "+0.666666" @?=
                   0x666666 % 0x1000000,
    "-0.666666" ~: hexLiteral "-0.666666" @?=
                   -(0x666666 % 0x1000000),
    "0.7777777" ~: hexLiteral "0.7777777" @?=
                   0x7777777 % 0x10000000,
    "+0.7777777" ~: hexLiteral "+0.7777777" @?=
                    0x7777777 % 0x10000000,
    "-0.7777777" ~: hexLiteral "-0.7777777" @?=
                    -(0x7777777 % 0x10000000),
    "0.88888888" ~: hexLiteral "0.88888888" @?=
                    0x88888888 % 0x100000000,
    "+0.88888888" ~: hexLiteral "+0.88888888" @?=
                     0x88888888 % 0x100000000,
    "-0.88888888" ~: hexLiteral "-0.88888888" @?=
                     -(0x88888888 % 0x100000000),
    "0.999999999" ~: hexLiteral "0.999999999" @?=
                     0x999999999 % 0x1000000000,
    "+0.999999999" ~: hexLiteral "+0.999999999" @?=
                     0x999999999 % 0x1000000000,
    "-0.999999999" ~: hexLiteral "-0.999999999" @?=
                     -(0x999999999 % 0x1000000000),
    "0.aaaaaaaaaa" ~: hexLiteral "0.aaaaaaaaaa" @?=
                      0xaaaaaaaaaa % 0x10000000000,
    "+0.aaaaaaaaaa" ~: hexLiteral "+0.aaaaaaaaaa" @?=
                       0xaaaaaaaaaa % 0x10000000000,
    "-0.aaaaaaaaaa" ~: hexLiteral "-0.aaaaaaaaaa" @?=
                       -(0xaaaaaaaaaa % 0x10000000000),
    "0.bbbbbbbbbbb" ~: hexLiteral "0.bbbbbbbbbbb" @?=
                       0xbbbbbbbbbbb % 0x100000000000,
    "+0.bbbbbbbbbbb" ~: hexLiteral "+0.bbbbbbbbbbb" @?=
                        0xbbbbbbbbbbb % 0x100000000000,
    "-0.bbbbbbbbbbb" ~: hexLiteral "-0.bbbbbbbbbbb" @?=
                        -(0xbbbbbbbbbbb % 0x100000000000),
    "0.cccccccccccc" ~: hexLiteral "0.cccccccccccc" @?=
                        0xcccccccccccc % 0x1000000000000,
    "+0.cccccccccccc" ~: hexLiteral "+0.cccccccccccc" @?=
                         0xcccccccccccc % 0x1000000000000,
    "-0.cccccccccccc" ~: hexLiteral "-0.cccccccccccc" @?=
                         -(0xcccccccccccc % 0x1000000000000),
    "0.ddddddddddddd" ~: hexLiteral "0.ddddddddddddd" @?=
                         0xddddddddddddd % 0x10000000000000,
    "+0.ddddddddddddd" ~: hexLiteral "+0.ddddddddddddd" @?=
                          0xddddddddddddd % 0x10000000000000,
    "-0.ddddddddddddd" ~: hexLiteral "-0.ddddddddddddd" @?=
                          -(0xddddddddddddd % 0x10000000000000),
    "0.eeeeeeeeeeeeee" ~: hexLiteral "0.eeeeeeeeeeeeee" @?=
                          0xeeeeeeeeeeeeee % 0x100000000000000,
    "+0.eeeeeeeeeeeeee" ~: hexLiteral "+0.eeeeeeeeeeeeee" @?=
                           0xeeeeeeeeeeeeee % 0x100000000000000,
    "-0.eeeeeeeeeeeeee" ~: hexLiteral "-0.eeeeeeeeeeeeee" @?=
                           -(0xeeeeeeeeeeeeee % 0x100000000000000),
    "0.fffffffffffffff" ~: hexLiteral "0.fffffffffffffff" @?=
                           0xfffffffffffffff % 0x1000000000000000,
    "+0.fffffffffffffff" ~: hexLiteral "+0.fffffffffffffff" @?=
                            0xfffffffffffffff % 0x1000000000000000,
    "-0.fffffffffffffff" ~: hexLiteral "-0.fffffffffffffff" @?=
                            -(0xfffffffffffffff % 0x1000000000000000),
    "1.2" ~: hexLiteral "1.2" @?=
             0x12 % 0x10,
    "+1.2" ~: hexLiteral "+1.2" @?=
              0x12 % 0x10,
    "-1.2" ~: hexLiteral "-1.2" @?=
              -(0x12 % 0x10),
    "12.34" ~: hexLiteral "12.34" @?=
              0x1234 % 0x100,
    "+12.34" ~: hexLiteral "+12.34" @?=
               0x1234 % 0x100,
    "-12.34" ~: hexLiteral "-12.34" @?=
               -(0x1234 % 0x100),
    "123.456" ~: hexLiteral "123.456" @?=
               0x123456 % 0x1000,
    "+123.456" ~: hexLiteral "+123.456" @?=
                0x123456 % 0x1000,
    "-123.456" ~: hexLiteral "-123.456" @?=
                -0x123456 % 0x1000,
    "1234.5678" ~: hexLiteral "1234.5678" @?=
                0x12345678 % 0x10000,
    "+1234.5678" ~: hexLiteral "+1234.5678" @?=
                 0x12345678 % 0x10000,
    "-1234.5678" ~: hexLiteral "-1234.5678" @?=
                 -0x12345678 % 0x10000,
    "12345.6789a" ~: hexLiteral "12345.6789a" @?=
                 0x123456789a % 0x100000,
    "+12345.6789a" ~: hexLiteral "+12345.6789a" @?=
                  0x123456789a % 0x100000,
    "-12345.6789a" ~: hexLiteral "-12345.6789a" @?=
                  -(0x123456789a % 0x100000),
    "123456.789abc" ~: hexLiteral "123456.789abc" @?=
                  0x123456789abc % 0x1000000,
    "+123456.789abc" ~: hexLiteral "+123456.789abc" @?=
                   0x123456789abc % 0x1000000,
    "-123456.789abc" ~: hexLiteral "-123456.789abc" @?=
                   -(0x123456789abc % 0x1000000),
    "1234567.89abcde" ~: hexLiteral "1234567.89abcde" @?=
                   0x123456789abcde % 0x10000000,
    "+1234567.89abcde" ~: hexLiteral "+1234567.89abcde" @?=
                    0x123456789abcde % 0x10000000,
    "-1234567.89abcde" ~: hexLiteral "-1234567.89abcde" @?=
                    -(0x123456789abcde % 0x10000000),
    "1234567.89abcdef" ~: hexLiteral "1234567.89abcdef" @?=
                   0x123456789abcdef % 0x100000000,
    "+1234567.89abcdef" ~: hexLiteral "+1234567.89abcdef" @?=
                    0x123456789abcdef % 0x100000000,
    "-1234567.89abcdef" ~: hexLiteral "-1234567.89abcdef" @?=
                    -(0x123456789abcdef % 0x100000000),
    "1.2p1" ~: hexLiteral "1.2p1" @?=
             0x12,
    "+1.2p1" ~: hexLiteral "+1.2p1" @?=
              0x12,
    "-1.2p1" ~: hexLiteral "-1.2p1" @?=
              -0x12,
    "12.34p1" ~: hexLiteral "12.34p1" @?=
              0x1234 % 0x10,
    "+12.34p1" ~: hexLiteral "+12.34p1" @?=
               0x1234 % 0x10,
    "-12.34p1" ~: hexLiteral "-12.34p1" @?=
               -(0x1234 % 0x10),
    "123.456p1" ~: hexLiteral "123.456p1" @?=
               0x123456 % 0x100,
    "+123.456p1" ~: hexLiteral "+123.456p1" @?=
                0x123456 % 0x100,
    "-123.456p1" ~: hexLiteral "-123.456p1" @?=
                -0x123456 % 0x100,
    "1234.5678p1" ~: hexLiteral "1234.5678p1" @?=
                0x12345678 % 0x1000,
    "+1234.5678p1" ~: hexLiteral "+1234.5678p1" @?=
                 0x12345678 % 0x1000,
    "-1234.5678p1" ~: hexLiteral "-1234.5678p1" @?=
                 -0x12345678 % 0x1000,
    "12345.6789ap1" ~: hexLiteral "12345.6789ap1" @?=
                 0x123456789a % 0x10000,
    "+12345.6789ap1" ~: hexLiteral "+12345.6789ap1" @?=
                  0x123456789a % 0x10000,
    "-12345.6789ap1" ~: hexLiteral "-12345.6789ap1" @?=
                  -(0x123456789a % 0x10000),
    "123456.789abcp1" ~: hexLiteral "123456.789abcp1" @?=
                  0x123456789abc % 0x100000,
    "+123456.789abcp1" ~: hexLiteral "+123456.789abcp1" @?=
                   0x123456789abc % 0x100000,
    "-123456.789abcp1" ~: hexLiteral "-123456.789abcp1" @?=
                   -(0x123456789abc % 0x100000),
    "1234567.89abcdep1" ~: hexLiteral "1234567.89abcdep1" @?=
                   0x123456789abcde % 0x1000000,
    "+1234567.89abcdep1" ~: hexLiteral "+1234567.89abcdep1" @?=
                    0x123456789abcde % 0x1000000,
    "-1234567.89abcdep1" ~: hexLiteral "-1234567.89abcdep1" @?=
                    -(0x123456789abcde % 0x1000000),
    "1234567.89abcdefp1" ~: hexLiteral "1234567.89abcdefp1" @?=
                   0x123456789abcdef % 0x10000000,
    "+1234567.89abcdefp1" ~: hexLiteral "+1234567.89abcdefp1" @?=
                    0x123456789abcdef % 0x10000000,
    "-1234567.89abcdefp1" ~: hexLiteral "-1234567.89abcdefp1" @?=
                    -(0x123456789abcdef % 0x10000000),
    "1.2p-1" ~: hexLiteral "1.2p-1" @?=
             0x12 % 0x100,
    "+1.2p-1" ~: hexLiteral "+1.2p-1" @?=
              0x12 % 0x100,
    "-1.2p-1" ~: hexLiteral "-1.2p-1" @?=
              -(0x12 % 0x100),
    "12.34p-1" ~: hexLiteral "12.34p-1" @?=
              0x1234 % 0x1000,
    "+12.34p-1" ~: hexLiteral "+12.34p-1" @?=
               0x1234 % 0x1000,
    "-12.34p-1" ~: hexLiteral "-12.34p-1" @?=
               -(0x1234 % 0x1000),
    "123.456p-1" ~: hexLiteral "123.456p-1" @?=
               0x123456 % 0x10000,
    "+123.456p-1" ~: hexLiteral "+123.456p-1" @?=
                0x123456 % 0x10000,
    "-123.456p-1" ~: hexLiteral "-123.456p-1" @?=
                -0x123456 % 0x10000,
    "1234.5678p-1" ~: hexLiteral "1234.5678p-1" @?=
                0x12345678 % 0x100000,
    "+1234.5678p-1" ~: hexLiteral "+1234.5678p-1" @?=
                 0x12345678 % 0x100000,
    "-1234.5678p-1" ~: hexLiteral "-1234.5678p-1" @?=
                 -0x12345678 % 0x100000,
    "12345.6789ap-1" ~: hexLiteral "12345.6789ap-1" @?=
                 0x123456789a % 0x1000000,
    "+12345.6789ap-1" ~: hexLiteral "+12345.6789ap-1" @?=
                  0x123456789a % 0x1000000,
    "-12345.6789ap-1" ~: hexLiteral "-12345.6789ap-1" @?=
                  -(0x123456789a % 0x1000000),
    "123456.789abcp-1" ~: hexLiteral "123456.789abcp-1" @?=
                  0x123456789abc % 0x10000000,
    "+123456.789abcp-1" ~: hexLiteral "+123456.789abcp-1" @?=
                   0x123456789abc % 0x10000000,
    "-123456.789abcp-1" ~: hexLiteral "-123456.789abcp-1" @?=
                   -(0x123456789abc % 0x10000000),
    "1234567.89abcdep-1" ~: hexLiteral "1234567.89abcdep-1" @?=
                   0x123456789abcde % 0x100000000,
    "+1234567.89abcdep-1" ~: hexLiteral "+1234567.89abcdep-1" @?=
                    0x123456789abcde % 0x100000000,
    "-1234567.89abcdep-1" ~: hexLiteral "-1234567.89abcdep-1" @?=
                    -(0x123456789abcde % 0x100000000),
    "1234567.89abcdefp-1" ~: hexLiteral "1234567.89abcdefp-1" @?=
                   0x123456789abcdef % 0x1000000000,
    "+1234567.89abcdefp-1" ~: hexLiteral "+1234567.89abcdefp-1" @?=
                    0x123456789abcdef % 0x1000000000,
    "-1234567.89abcdefp-1" ~: hexLiteral "-1234567.89abcdefp-1" @?=
                    -(0x123456789abcdef % 0x1000000000),
    "12pa" ~: hexLiteral "12pa" @?=
             0x120000000000,
    "+12pa" ~: hexLiteral "+12pa" @?=
              0x120000000000,
    "-12pa" ~: hexLiteral "-12pa" @?=
              -0x120000000000,
    "1234pa" ~: hexLiteral "1234pa" @?=
              0x12340000000000,
    "+1234pa" ~: hexLiteral "+1234pa" @?=
               0x12340000000000,
    "-1234pa" ~: hexLiteral "-1234pa" @?=
               -0x12340000000000,
    "123456pa" ~: hexLiteral "123456pa" @?=
               0x1234560000000000,
    "+123456pa" ~: hexLiteral "+123456pa" @?=
                0x1234560000000000,
    "-123456pa" ~: hexLiteral "-123456pa" @?=
                -0x1234560000000000,
    "12345678pa" ~: hexLiteral "12345678pa" @?=
                0x123456780000000000,
    "+12345678pa" ~: hexLiteral "+12345678pa" @?=
                 0x123456780000000000,
    "-12345678pa" ~: hexLiteral "-12345678pa" @?=
                 -0x123456780000000000,
    "123456789apa" ~: hexLiteral "123456789apa" @?=
                 0x123456789a0000000000,
    "+123456789apa" ~: hexLiteral "+123456789apa" @?=
                  0x123456789a0000000000,
    "-123456789apa" ~: hexLiteral "-123456789apa" @?=
                  -0x123456789a0000000000,
    "123456789abcpa" ~: hexLiteral "123456789abcpa" @?=
                  0x123456789abc0000000000,
    "+123456789abcpa" ~: hexLiteral "+123456789abcpa" @?=
                   0x123456789abc0000000000,
    "-123456789abcpa" ~: hexLiteral "-123456789abcpa" @?=
                   -0x123456789abc0000000000,
    "123456789abcdepa" ~: hexLiteral "123456789abcdepa" @?=
                   0x123456789abcde0000000000,
    "+123456789abcdepa" ~: hexLiteral "+123456789abcdepa" @?=
                    0x123456789abcde0000000000,
    "-123456789abcdepa" ~: hexLiteral "-123456789abcdepa" @?=
                    -0x123456789abcde0000000000,
    "123456789abcdefpa" ~: hexLiteral "123456789abcdefpa" @?=
                   0x123456789abcdef0000000000,
    "+123456789abcdefpa" ~: hexLiteral "+123456789abcdefpa" @?=
                    0x123456789abcdef0000000000,
    "-123456789abcdefpa" ~: hexLiteral "-123456789abcdefpa" @?=
                    -0x123456789abcdef0000000000,
    "12p-a" ~: hexLiteral "12p-a" @?=
             0x12 % 0x10000000000,
    "+12p-a" ~: hexLiteral "+12p-a" @?=
              0x12 % 0x10000000000,
    "-12p-a" ~: hexLiteral "-12p-a" @?=
              -(0x12 % 0x10000000000),
    "1234p-a" ~: hexLiteral "1234p-a" @?=
              0x1234 % 0x10000000000,
    "+1234p-a" ~: hexLiteral "+1234p-a" @?=
               0x1234 % 0x10000000000,
    "-1234p-a" ~: hexLiteral "-1234p-a" @?=
               -(0x1234 % 0x10000000000),
    "123456p-a" ~: hexLiteral "123456p-a" @?=
               0x123456 % 0x10000000000,
    "+123456p-a" ~: hexLiteral "+123456p-a" @?=
                0x123456 % 0x10000000000,
    "-123456p-a" ~: hexLiteral "-123456p-a" @?=
                -0x123456 % 0x10000000000,
    "12345678p-a" ~: hexLiteral "12345678p-a" @?=
                0x12345678 % 0x10000000000,
    "+12345678p-a" ~: hexLiteral "+12345678p-a" @?=
                 0x12345678 % 0x10000000000,
    "-12345678p-a" ~: hexLiteral "-12345678p-a" @?=
                 -0x12345678 % 0x10000000000,
    "123456789ap-a" ~: hexLiteral "123456789ap-a" @?=
                 0x123456789a % 0x10000000000,
    "+123456789ap-a" ~: hexLiteral "+123456789ap-a" @?=
                  0x123456789a % 0x10000000000,
    "-123456789ap-a" ~: hexLiteral "-123456789ap-a" @?=
                  -(0x123456789a % 0x10000000000),
    "123456789abcp-a" ~: hexLiteral "123456789abcp-a" @?=
                  0x123456789abc % 0x10000000000,
    "+123456789abcp-a" ~: hexLiteral "+123456789abcp-a" @?=
                   0x123456789abc % 0x10000000000,
    "-123456789abcp-a" ~: hexLiteral "-123456789abcp-a" @?=
                   -(0x123456789abc % 0x10000000000),
    "123456789abcdep-a" ~: hexLiteral "123456789abcdep-a" @?=
                   0x123456789abcde % 0x10000000000,
    "+123456789abcdep-a" ~: hexLiteral "+123456789abcdep-a" @?=
                    0x123456789abcde % 0x10000000000,
    "-123456789abcdep-a" ~: hexLiteral "-123456789abcdep-a" @?=
                    -(0x123456789abcde % 0x10000000000),
    "123456789abcdefp-a" ~: hexLiteral "123456789abcdefp-a" @?=
                   0x123456789abcdef % 0x10000000000,
    "+123456789abcdefp-a" ~: hexLiteral "+123456789abcdefp-a" @?=
                    0x123456789abcdef % 0x10000000000,
    "-123456789abcdefp-a" ~: hexLiteral "-123456789abcdefp-a" @?=
                    -(0x123456789abcdef % 0x10000000000),
    "1.2pa" ~: hexLiteral "1.2pa" @?=
             0x12000000000,
    "+1.2pa" ~: hexLiteral "+1.2pa" @?=
              0x12000000000,
    "-1.2pa" ~: hexLiteral "-1.2pa" @?=
              -0x12000000000,
    "12.34pa" ~: hexLiteral "12.34pa" @?=
              0x123400000000,
    "+12.34pa" ~: hexLiteral "+12.34pa" @?=
               0x123400000000,
    "-12.34pa" ~: hexLiteral "-12.34pa" @?=
               -0x123400000000,
    "123.456pa" ~: hexLiteral "123.456pa" @?=
               0x1234560000000,
    "+123.456pa" ~: hexLiteral "+123.456pa" @?=
                0x1234560000000,
    "-123.456pa" ~: hexLiteral "-123.456pa" @?=
                -0x1234560000000,
    "1234.5678pa" ~: hexLiteral "1234.5678pa" @?=
                0x12345678000000,
    "+1234.5678pa" ~: hexLiteral "+1234.5678pa" @?=
                 0x12345678000000,
    "-1234.5678pa" ~: hexLiteral "-1234.5678pa" @?=
                 -0x12345678000000,
    "12345.6789apa" ~: hexLiteral "12345.6789apa" @?=
                 0x123456789a00000,
    "+12345.6789apa" ~: hexLiteral "+12345.6789apa" @?=
                  0x123456789a00000,
    "-12345.6789apa" ~: hexLiteral "-12345.6789apa" @?=
                  -0x123456789a00000,
    "123456.789abcpa" ~: hexLiteral "123456.789abcpa" @?=
                  0x123456789abc0000,
    "+123456.789abcpa" ~: hexLiteral "+123456.789abcpa" @?=
                   0x123456789abc0000,
    "-123456.789abcpa" ~: hexLiteral "-123456.789abcpa" @?=
                   -0x123456789abc0000,
    "1234567.89abcdepa" ~: hexLiteral "1234567.89abcdepa" @?=
                   0x123456789abcde000,
    "+1234567.89abcdepa" ~: hexLiteral "+1234567.89abcdepa" @?=
                    0x123456789abcde000,
    "-1234567.89abcdepa" ~: hexLiteral "-1234567.89abcdepa" @?=
                    -0x123456789abcde000,
    "1234567.89abcdefpa" ~: hexLiteral "1234567.89abcdefpa" @?=
                   0x123456789abcdef00,
    "+1234567.89abcdefpa" ~: hexLiteral "+1234567.89abcdefpa" @?=
                    0x123456789abcdef00,
    "-1234567.89abcdefpa" ~: hexLiteral "-1234567.89abcdefpa" @?=
                    -0x123456789abcdef00,
    "1.2p-a" ~: hexLiteral "1.2p-a" @?=
             0x12 % 0x100000000000,
    "+1.2p-a" ~: hexLiteral "+1.2p-a" @?=
              0x12 % 0x100000000000,
    "-1.2p-a" ~: hexLiteral "-1.2p-a" @?=
              -(0x12 % 0x100000000000),
    "12.34p-a" ~: hexLiteral "12.34p-a" @?=
              0x1234 % 0x1000000000000,
    "+12.34p-a" ~: hexLiteral "+12.34p-a" @?=
               0x1234 % 0x1000000000000,
    "-12.34p-a" ~: hexLiteral "-12.34p-a" @?=
               -(0x1234 % 0x1000000000000),
    "123.456p-a" ~: hexLiteral "123.456p-a" @?=
               0x123456 % 0x10000000000000,
    "+123.456p-a" ~: hexLiteral "+123.456p-a" @?=
                0x123456 % 0x10000000000000,
    "-123.456p-a" ~: hexLiteral "-123.456p-a" @?=
                -0x123456 % 0x10000000000000,
    "1234.5678p-a" ~: hexLiteral "1234.5678p-a" @?=
                0x12345678 % 0x100000000000000,
    "+1234.5678p-a" ~: hexLiteral "+1234.5678p-a" @?=
                 0x12345678 % 0x100000000000000,
    "-1234.5678p-a" ~: hexLiteral "-1234.5678p-a" @?=
                 -0x12345678 % 0x100000000000000,
    "12345.6789ap-a" ~: hexLiteral "12345.6789ap-a" @?=
                 0x123456789a % 0x1000000000000000,
    "+12345.6789ap-a" ~: hexLiteral "+12345.6789ap-a" @?=
                  0x123456789a % 0x1000000000000000,
    "-12345.6789ap-a" ~: hexLiteral "-12345.6789ap-a" @?=
                  -(0x123456789a % 0x1000000000000000),
    "123456.789abcp-a" ~: hexLiteral "123456.789abcp-a" @?=
                  0x123456789abc % 0x10000000000000000,
    "+123456.789abcp-a" ~: hexLiteral "+123456.789abcp-a" @?=
                   0x123456789abc % 0x10000000000000000,
    "-123456.789abcp-a" ~: hexLiteral "-123456.789abcp-a" @?=
                   -(0x123456789abc % 0x10000000000000000),
    "1234567.89abcdep-a" ~: hexLiteral "1234567.89abcdep-a" @?=
                   0x123456789abcde % 0x100000000000000000,
    "+1234567.89abcdep-a" ~: hexLiteral "+1234567.89abcdep-a" @?=
                    0x123456789abcde % 0x100000000000000000,
    "-1234567.89abcdep-a" ~: hexLiteral "-1234567.89abcdep-a" @?=
                    -(0x123456789abcde % 0x100000000000000000),
    "1234567.89abcdefp-a" ~: hexLiteral "1234567.89abcdefp-a" @?=
                   0x123456789abcdef % 0x1000000000000000000,
    "+1234567.89abcdefp-a" ~: hexLiteral "+1234567.89abcdefp-a" @?=
                    0x123456789abcdef % 0x1000000000000000000,
    "-1234567.89abcdefp-a" ~: hexLiteral "-1234567.89abcdefp-a" @?=
                    -(0x123456789abcdef % 0x1000000000000000000)
    ]

testDecLiteral = [
    "0" ~: decLiteral "0" @?= 0,
    "+0" ~: decLiteral "+0" @?= 0,
    "-0" ~: decLiteral "-0" @?= 0,
    "1" ~: decLiteral "1" @?= 1,
    "+1" ~: decLiteral "+1" @?= 1,
    "-1" ~: decLiteral "-1" @?= -1,
    "22" ~: decLiteral "22" @?= 22,
    "+22" ~: decLiteral "+22" @?= 22,
    "-22" ~: decLiteral "-22" @?= -22,
    "333" ~: decLiteral "333" @?= 333,
    "+333" ~: decLiteral "+333" @?= 333,
    "-333" ~: decLiteral "-333" @?= -333,
    "4444" ~: decLiteral "4444" @?= 4444,
    "+4444" ~: decLiteral "+4444" @?= 4444,
    "-4444" ~: decLiteral "-4444" @?= -4444,
    "55555" ~: decLiteral "55555" @?= 55555,
    "+55555" ~: decLiteral "+55555" @?= 55555,
    "-55555" ~: decLiteral "-55555" @?= -55555,
    "666666" ~: decLiteral "666666" @?= 666666,
    "+666666" ~: decLiteral "+666666" @?= 666666,
    "-666666" ~: decLiteral "-666666" @?= -666666,
    "7777777" ~: decLiteral "7777777" @?= 7777777,
    "+7777777" ~: decLiteral "+7777777" @?= 7777777,
    "-7777777" ~: decLiteral "-7777777" @?= -7777777,
    "88888888" ~: decLiteral "88888888" @?= 88888888,
    "+88888888" ~: decLiteral "+88888888" @?= 88888888,
    "-88888888" ~: decLiteral "-88888888" @?= -88888888,
    "999999999" ~: decLiteral "999999999" @?= 999999999,
    "+999999999" ~: decLiteral "+999999999" @?= 999999999,
    "-999999999" ~: decLiteral "-999999999" @?= -999999999,
    "0.0" ~: decLiteral "0.0" @?=
             0,
    "+0.0" ~: decLiteral "+0.0" @?=
              0,
    "-0.0" ~: decLiteral "-0.0" @?=
              0,
    "0.1" ~: decLiteral "0.1" @?=
             1 % 10,
    "+0.1" ~: decLiteral "+0.1" @?=
              1 % 10,
    "-0.1" ~: decLiteral "-0.1" @?=
              -(1 % 10),
    "0.22" ~: decLiteral "0.22" @?=
              22 % 100,
    "+0.22" ~: decLiteral "+0.22" @?=
               22 % 100,
    "-0.22" ~: decLiteral "-0.22" @?=
               -(22 % 100),
    "0.333" ~: decLiteral "0.333" @?=
               333 % 1000,
    "+0.333" ~: decLiteral "+0.333" @?=
                333 % 1000,
    "-0.333" ~: decLiteral "-0.333" @?=
                -333 % 1000,
    "0.4444" ~: decLiteral "0.4444" @?=
                4444 % 10000,
    "+0.4444" ~: decLiteral "+0.4444" @?=
                 4444 % 10000,
    "-0.4444" ~: decLiteral "-0.4444" @?=
                 -4444 % 10000,
    "0.55555" ~: decLiteral "0.55555" @?=
                 55555 % 100000,
    "+0.55555" ~: decLiteral "+0.55555" @?=
                  55555 % 100000,
    "-0.55555" ~: decLiteral "-0.55555" @?=
                  -(55555 % 100000),
    "0.666666" ~: decLiteral "0.666666" @?=
                  666666 % 1000000,
    "+0.666666" ~: decLiteral "+0.666666" @?=
                   666666 % 1000000,
    "-0.666666" ~: decLiteral "-0.666666" @?=
                   -(666666 % 1000000),
    "0.7777777" ~: decLiteral "0.7777777" @?=
                   7777777 % 10000000,
    "+0.7777777" ~: decLiteral "+0.7777777" @?=
                    7777777 % 10000000,
    "-0.7777777" ~: decLiteral "-0.7777777" @?=
                    -(7777777 % 10000000),
    "0.88888888" ~: decLiteral "0.88888888" @?=
                    88888888 % 100000000,
    "+0.88888888" ~: decLiteral "+0.88888888" @?=
                     88888888 % 100000000,
    "-0.88888888" ~: decLiteral "-0.88888888" @?=
                     -(88888888 % 100000000),
    "0.999999999" ~: decLiteral "0.999999999" @?=
                     999999999 % 1000000000,
    "+0.999999999" ~: decLiteral "+0.999999999" @?=
                     999999999 % 1000000000,
    "-0.999999999" ~: decLiteral "-0.999999999" @?=
                     -(999999999 % 1000000000),
    "1.2" ~: decLiteral "1.2" @?=
             12 % 10,
    "+1.2" ~: decLiteral "+1.2" @?=
              12 % 10,
    "-1.2" ~: decLiteral "-1.2" @?=
              -(12 % 10),
    "12.34" ~: decLiteral "12.34" @?=
              1234 % 100,
    "+12.34" ~: decLiteral "+12.34" @?=
               1234 % 100,
    "-12.34" ~: decLiteral "-12.34" @?=
               -(1234 % 100),
    "123.456" ~: decLiteral "123.456" @?=
               123456 % 1000,
    "+123.456" ~: decLiteral "+123.456" @?=
                123456 % 1000,
    "-123.456" ~: decLiteral "-123.456" @?=
                -123456 % 1000,
    "1234.5678" ~: decLiteral "1234.5678" @?=
                12345678 % 10000,
    "+1234.5678" ~: decLiteral "+1234.5678" @?=
                 12345678 % 10000,
    "-1234.5678" ~: decLiteral "-1234.5678" @?=
                 -12345678 % 10000,
    "12345.6789" ~: decLiteral "12345.6789" @?=
                 123456789 % 10000,
    "+12345.6789" ~: decLiteral "+12345.6789" @?=
                  123456789 % 10000,
    "-12345.6789" ~: decLiteral "-12345.6789" @?=
                  -(123456789 % 10000),
    "1.2e1" ~: decLiteral "1.2e1" @?=
               12,
    "+1.2e1" ~: decLiteral "+1.2e1" @?=
                12,
    "-1.2e1" ~: decLiteral "-1.2e1" @?=
                -12,
    "12.34e1" ~: decLiteral "12.34e1" @?=
              1234 % 10,
    "+12.34e1" ~: decLiteral "+12.34e1" @?=
               1234 % 10,
    "-12.34e1" ~: decLiteral "-12.34e1" @?=
               -(1234 % 10),
    "123.456e1" ~: decLiteral "123.456e1" @?=
               123456 % 100,
    "+123.456e1" ~: decLiteral "+123.456e1" @?=
                123456 % 100,
    "-123.456e1" ~: decLiteral "-123.456e1" @?=
                -123456 % 100,
    "1234.5678e1" ~: decLiteral "1234.5678e1" @?=
                12345678 % 1000,
    "+1234.5678e1" ~: decLiteral "+1234.5678e1" @?=
                 12345678 % 1000,
    "-1234.5678e1" ~: decLiteral "-1234.5678e1" @?=
                 -12345678 % 1000,
    "12345.6789e1" ~: decLiteral "12345.6789e1" @?=
                 123456789 % 1000,
    "+12345.6789e1" ~: decLiteral "+12345.6789e1" @?=
                  123456789 % 1000,
    "-12345.6789e1" ~: decLiteral "-12345.6789e1" @?=
                  -(123456789 % 1000),
    "1.2e-1" ~: decLiteral "1.2e-1" @?=
             12 % 100,
    "+1.2e-1" ~: decLiteral "+1.2e-1" @?=
              12 % 100,
    "-1.2e-1" ~: decLiteral "-1.2e-1" @?=
              -(12 % 100),
    "12.34e-1" ~: decLiteral "12.34e-1" @?=
              1234 % 1000,
    "+12.34e-1" ~: decLiteral "+12.34e-1" @?=
               1234 % 1000,
    "-12.34e-1" ~: decLiteral "-12.34e-1" @?=
               -(1234 % 1000),
    "123.456e-1" ~: decLiteral "123.456e-1" @?=
               123456 % 10000,
    "+123.456e-1" ~: decLiteral "+123.456e-1" @?=
                123456 % 10000,
    "-123.456e-1" ~: decLiteral "-123.456e-1" @?=
                -123456 % 10000,
    "1234.5678e-1" ~: decLiteral "1234.5678e-1" @?=
                12345678 % 100000,
    "+1234.5678e-1" ~: decLiteral "+1234.5678e-1" @?=
                 12345678 % 100000,
    "-1234.5678e-1" ~: decLiteral "-1234.5678e-1" @?=
                 -12345678 % 100000,
    "12345.6789e-1" ~: decLiteral "12345.6789e-1" @?=
                 123456789 % 100000,
    "+12345.6789e-1" ~: decLiteral "+12345.6789e-1" @?=
                  123456789 % 100000,
    "-12345.6789e-1" ~: decLiteral "-12345.6789e-1" @?=
                  -(123456789 % 100000),
    "12e5" ~: decLiteral "12e5" @?=
               1200000,
    "+12e5" ~: decLiteral "+12e5" @?=
                1200000,
    "-12e5" ~: decLiteral "-12e5" @?=
                -1200000,
    "1234e5" ~: decLiteral "1234e5" @?=
              123400000,
    "+1234e5" ~: decLiteral "+1234e5" @?=
               123400000,
    "-1234e5" ~: decLiteral "-1234e5" @?=
               -123400000,
    "123456e5" ~: decLiteral "123456e5" @?=
               12345600000,
    "+123456e5" ~: decLiteral "+123456e5" @?=
                12345600000,
    "-123456e5" ~: decLiteral "-123456e5" @?=
                -12345600000,
    "12345678e5" ~: decLiteral "12345678e5" @?=
                1234567800000,
    "+12345678e5" ~: decLiteral "+12345678e5" @?=
                 1234567800000,
    "-12345678e5" ~: decLiteral "-12345678e5" @?=
                 -1234567800000,
    "123456789e5" ~: decLiteral "123456789e5" @?=
                 12345678900000,
    "+123456789e5" ~: decLiteral "+123456789e5" @?=
                  12345678900000,
    "-123456789e5" ~: decLiteral "-123456789e5" @?=
                  -12345678900000,
    "12e-5" ~: decLiteral "12e-5" @?=
             12 % 100000,
    "+12e-5" ~: decLiteral "+12e-5" @?=
              12 % 100000,
    "-12e-5" ~: decLiteral "-12e-5" @?=
              -(12 % 100000),
    "1234e-5" ~: decLiteral "1234e-5" @?=
              1234 % 100000,
    "+1234e-5" ~: decLiteral "+1234e-5" @?=
               1234 % 100000,
    "-1234e-5" ~: decLiteral "-1234e-5" @?=
               -(1234 % 100000),
    "123456e-5" ~: decLiteral "123456e-5" @?=
               123456 % 100000,
    "+123456e-5" ~: decLiteral "+123456e-5" @?=
                123456 % 100000,
    "-123456e-5" ~: decLiteral "-123456e-5" @?=
                -123456 % 100000,
    "12345678e-5" ~: decLiteral "12345678e-5" @?=
                12345678 % 100000,
    "+12345678e-5" ~: decLiteral "+12345678e-5" @?=
                 12345678 % 100000,
    "-12345678e-5" ~: decLiteral "-12345678e-5" @?=
                 -12345678 % 100000,
    "123456789e-5" ~: decLiteral "123456789e-5" @?=
                 123456789 % 100000,
    "+123456789e-5" ~: decLiteral "+123456789e-5" @?=
                  123456789 % 100000,
    "-123456789e-5" ~: decLiteral "-123456789e-5" @?=
                  -(123456789 % 100000),
    "1.2e5" ~: decLiteral "1.2e5" @?=
               120000,
    "+1.2e5" ~: decLiteral "+1.2e5" @?=
                120000,
    "-1.2e5" ~: decLiteral "-1.2e5" @?=
                -120000,
    "12.34e5" ~: decLiteral "12.34e5" @?=
              1234000,
    "+12.34e5" ~: decLiteral "+12.34e5" @?=
               1234000,
    "-12.34e5" ~: decLiteral "-12.34e5" @?=
               -1234000,
    "123.456e5" ~: decLiteral "123.456e5" @?=
               12345600,
    "+123.456e5" ~: decLiteral "+123.456e5" @?=
                12345600,
    "-123.456e5" ~: decLiteral "-123.456e5" @?=
                -12345600,
    "1234.5678e5" ~: decLiteral "1234.5678e5" @?=
                123456780,
    "+1234.5678e5" ~: decLiteral "+1234.5678e5" @?=
                 123456780,
    "-1234.5678e5" ~: decLiteral "-1234.5678e5" @?=
                 -123456780,
    "1234.56789e5" ~: decLiteral "1234.56789e5" @?=
                 123456789,
    "+1234.56789e5" ~: decLiteral "+1234.56789e5" @?=
                  123456789,
    "-1234.56789e5" ~: decLiteral "-1234.56789e5" @?=
                  -123456789,
    "1.2e-5" ~: decLiteral "1.2e-5" @?=
             12 % 1000000,
    "+1.2e-5" ~: decLiteral "+1.2e-5" @?=
              12 % 1000000,
    "-1.2e-5" ~: decLiteral "-1.2e-5" @?=
              -(12 % 1000000),
    "12.34e-5" ~: decLiteral "12.34e-5" @?=
              1234 % 10000000,
    "+12.34e-5" ~: decLiteral "+12.34e-5" @?=
               1234 % 10000000,
    "-12.34e-5" ~: decLiteral "-12.34e-5" @?=
               -(1234 % 10000000),
    "123.456e-5" ~: decLiteral "123.456e-5" @?=
               123456 % 100000000,
    "+123.456e-5" ~: decLiteral "+123.456e-5" @?=
                123456 % 100000000,
    "-123.456e-5" ~: decLiteral "-123.456e-5" @?=
                -123456 % 100000000,
    "1234.5678e-5" ~: decLiteral "1234.5678e-5" @?=
                12345678 % 1000000000,
    "+1234.5678e-5" ~: decLiteral "+1234.5678e-5" @?=
                 12345678 % 1000000000,
    "-1234.5678e-5" ~: decLiteral "-1234.5678e-5" @?=
                 -12345678 % 1000000000,
    "12345.6789e-5" ~: decLiteral "12345.6789e-5" @?=
                 123456789 % 1000000000,
    "+12345.6789e-5" ~: decLiteral "+12345.6789e-5" @?=
                  123456789 % 1000000000,
    "-12345.6789e-5" ~: decLiteral "-12345.6789e-5" @?=
                  -(123456789 % 1000000000)
  ]

testOctLiteral = [
    "0" ~: octLiteral "0" @?= 0,
    "+0" ~: octLiteral "+0" @?= 0,
    "-0" ~: octLiteral "-0" @?= 0,
    "1" ~: octLiteral "1" @?= 1,
    "+1" ~: octLiteral "+1" @?= 1,
    "-1" ~: octLiteral "-1" @?= -1,
    "22" ~: octLiteral "22" @?= 18,
    "+22" ~: octLiteral "+22" @?= 18,
    "-22" ~: octLiteral "-22" @?= -18,
    "333" ~: octLiteral "333" @?= 219,
    "+333" ~: octLiteral "+333" @?= 219,
    "-333" ~: octLiteral "-333" @?= -219,
    "4444" ~: octLiteral "4444" @?= 2340,
    "+4444" ~: octLiteral "+4444" @?= 2340,
    "-4444" ~: octLiteral "-4444" @?= -2340,
    "55555" ~: octLiteral "55555" @?= 23405,
    "+55555" ~: octLiteral "+55555" @?= 23405,
    "-55555" ~: octLiteral "-55555" @?= -23405,
    "666666" ~: octLiteral "666666" @?= 224694,
    "+666666" ~: octLiteral "+666666" @?= 224694,
    "-666666" ~: octLiteral "-666666" @?= -224694,
    "7777777" ~: octLiteral "7777777" @?= 2097151,
    "+7777777" ~: octLiteral "+7777777" @?= 2097151,
    "-7777777" ~: octLiteral "-7777777" @?= -2097151,
    "0.0" ~: octLiteral "0.0" @?=
             0,
    "+0.0" ~: octLiteral "+0.0" @?=
              0,
    "-0.0" ~: octLiteral "-0.0" @?=
              0,
    "0.1" ~: octLiteral "0.1" @?=
             1 % 8,
    "+0.1" ~: octLiteral "+0.1" @?=
              1 % 8,
    "-0.1" ~: octLiteral "-0.1" @?=
              -(1 % 8),
    "0.22" ~: octLiteral "0.22" @?=
              9 % 32,
    "+0.22" ~: octLiteral "+0.22" @?=
               9 % 32,
    "-0.22" ~: octLiteral "-0.22" @?=
               -(9 % 32),
    "0.333" ~: octLiteral "0.333" @?=
               219 % 512,
    "+0.333" ~: octLiteral "+0.333" @?=
                219 % 512,
    "-0.333" ~: octLiteral "-0.333" @?=
                -219 % 512,
    "0.4444" ~: octLiteral "0.4444" @?=
                585 % 1024,
    "+0.4444" ~: octLiteral "+0.4444" @?=
                 585 % 1024,
    "-0.4444" ~: octLiteral "-0.4444" @?=
                 -585 % 1024,
    "0.55555" ~: octLiteral "0.55555" @?=
                 23405 % 32768,
    "+0.55555" ~: octLiteral "+0.55555" @?=
                  23405 % 32768,
    "-0.55555" ~: octLiteral "-0.55555" @?=
                  -(23405 % 32768),
    "0.666666" ~: octLiteral "0.666666" @?=
                  112347 % 131072,
    "+0.666666" ~: octLiteral "+0.666666" @?=
                   112347 % 131072,
    "-0.666666" ~: octLiteral "-0.666666" @?=
                   -(112347 % 131072),
    "0.7777777" ~: octLiteral "0.7777777" @?=
                   2097151 % 2097152,
    "+0.7777777" ~: octLiteral "+0.7777777" @?=
                    2097151 % 2097152,
    "-0.7777777" ~: octLiteral "-0.7777777" @?=
                    -(2097151 % 2097152),
    "1.2" ~: octLiteral "1.2" @?=
             5 % 4,
    "+1.2" ~: octLiteral "+1.2" @?=
              5 % 4,
    "-1.2" ~: octLiteral "-1.2" @?=
              -(5 % 4),
    "12.34" ~: octLiteral "12.34" @?=
              167 % 16,
    "+12.34" ~: octLiteral "+12.34" @?=
               167 % 16,
    "-12.34" ~: octLiteral "-12.34" @?=
               -(167 % 16),
    "123.456" ~: octLiteral "123.456" @?=
               21399 % 256,
    "+123.456" ~: octLiteral "+123.456" @?=
                21399 % 256,
    "-123.456" ~: octLiteral "-123.456" @?=
                -21399 % 256,
    "1234.567" ~: octLiteral "1234.567" @?=
                342391 % 512,
    "+1234.567" ~: octLiteral "+1234.567" @?=
                 342391 % 512,
    "-1234.567" ~: octLiteral "-1234.567" @?=
                 -342391 % 512,
    "1.2p1" ~: octLiteral "1.2p1" @?=
             10,
    "+1.2p1" ~: octLiteral "+1.2p1" @?=
              10,
    "-1.2p1" ~: octLiteral "-1.2p1" @?=
              -10,
    "12.34p1" ~: octLiteral "12.34p1" @?=
              167 % 2,
    "+12.34p1" ~: octLiteral "+12.34p1" @?=
               167 % 2,
    "-12.34p1" ~: octLiteral "-12.34p1" @?=
               -(167 % 2),
    "123.456p1" ~: octLiteral "123.456p1" @?=
               21399 % 32,
    "+123.456p1" ~: octLiteral "+123.456p1" @?=
                21399 % 32,
    "-123.456p1" ~: octLiteral "-123.456p1" @?=
                -21399 % 32,
    "1234.567p1" ~: octLiteral "1234.567p1" @?=
                342391 % 64,
    "+1234.567p1" ~: octLiteral "+1234.567p1" @?=
                 342391 % 64,
    "-1234.567p1" ~: octLiteral "-1234.567p1" @?=
                 -342391 % 64,
    "1.2p-1" ~: octLiteral "1.2p-1" @?=
             5 % 32,
    "+1.2p-1" ~: octLiteral "+1.2p-1" @?=
              5 % 32,
    "-1.2p-1" ~: octLiteral "-1.2p-1" @?=
              -(5 % 32),
    "12.34p-1" ~: octLiteral "12.34p-1" @?=
              167 % 128,
    "+12.34p-1" ~: octLiteral "+12.34p-1" @?=
               167 % 128,
    "-12.34p-1" ~: octLiteral "-12.34p-1" @?=
               -(167 % 128),
    "123.456p-1" ~: octLiteral "123.456p-1" @?=
               21399 % 2048,
    "+123.456p-1" ~: octLiteral "+123.456p-1" @?=
                21399 % 2048,
    "-123.456p-1" ~: octLiteral "-123.456p-1" @?=
                -21399 % 2048,
    "1234.567p-1" ~: octLiteral "1234.567p-1" @?=
                342391 % 4096,
    "+1234.567p-1" ~: octLiteral "+1234.567p-1" @?=
                 342391 % 4096,
    "-1234.567p-1" ~: octLiteral "-1234.567p-1" @?=
                 -342391 % 4096,
    "12e6" ~: octLiteral "12e6" @?=
             2621440,
    "+12e6" ~: octLiteral "+12e6" @?=
              2621440,
    "-12e6" ~: octLiteral "-12e6" @?=
              -2621440,
    "1234e6" ~: octLiteral "1234e6" @?=
              175112192,
    "+1234e6" ~: octLiteral "+1234e6" @?=
               175112192,
    "-1234e6" ~: octLiteral "-1234e6" @?=
               -175112192,
    "123456e6" ~: octLiteral "123456e6" @?=
               11219238912,
    "+123456e6" ~: octLiteral "+123456e6" @?=
                11219238912,
    "-123456e6" ~: octLiteral "-123456e6" @?=
                -11219238912,
    "1234567e6" ~: octLiteral "1234567e6" @?=
                89755746304,
    "+1234567e6" ~: octLiteral "+1234567e6" @?=
                 89755746304,
    "-1234567e6" ~: octLiteral "-1234567e6" @?=
                 -89755746304,
    "12e-6" ~: octLiteral "12e-6" @?=
             5 % 131072,
    "+12e-6" ~: octLiteral "+12e-6" @?=
              5 % 131072,
    "-12e-6" ~: octLiteral "-12e-6" @?=
              -(5 % 131072),
    "1234e-6" ~: octLiteral "1234e-6" @?=
              167 % 65536,
    "+1234e-6" ~: octLiteral "+1234e-6" @?=
               167 % 65536,
    "-1234e-6" ~: octLiteral "-1234e-6" @?=
               -(167 % 65536),
    "123456e-6" ~: octLiteral "123456e-6" @?=
               21399 % 131072,
    "+123456e-6" ~: octLiteral "+123456e-6" @?=
                21399 % 131072,
    "-123456e-6" ~: octLiteral "-123456e-6" @?=
                -21399 % 131072,
    "1234567e-6" ~: octLiteral "1234567e-6" @?=
                342391 % 262144,
    "+1234567e-6" ~: octLiteral "+1234567e-6" @?=
                 342391 % 262144,
    "-1234567e-6" ~: octLiteral "-1234567e-6" @?=
                 -342391 % 262144,
    "1.2e6" ~: octLiteral "1.2e6" @?=
             327680,
    "+1.2e6" ~: octLiteral "+1.2e6" @?=
              327680,
    "-1.2e6" ~: octLiteral "-1.2e6" @?=
              -327680,
    "12.34e6" ~: octLiteral "12.34e6" @?=
              2736128,
    "+12.34e6" ~: octLiteral "+12.34e6" @?=
               2736128,
    "-12.34e6" ~: octLiteral "-12.34e6" @?=
               -2736128,
    "123.456e6" ~: octLiteral "123.456e6" @?=
               21912576,
    "+123.456e6" ~: octLiteral "+123.456e6" @?=
                21912576,
    "-123.456e6" ~: octLiteral "-123.456e6" @?=
                -21912576,
    "1234.567e6" ~: octLiteral "1234.567e6" @?=
                175304192,
    "+1234.567e6" ~: octLiteral "+1234.567e6" @?=
                 175304192,
    "-1234.567e6" ~: octLiteral "-1234.567e6" @?=
                 -175304192,
    "1.2e-6" ~: octLiteral "1.2e-6" @?=
             5 % 1048576,
    "+1.2e-6" ~: octLiteral "+1.2e-6" @?=
              5 % 1048576,
    "-1.2e-6" ~: octLiteral "-1.2e-6" @?=
              -(5 % 1048576),
    "12.34e-6" ~: octLiteral "12.34e-6" @?=
              167 % 4194304,
    "+12.34e-6" ~: octLiteral "+12.34e-6" @?=
               167 % 4194304,
    "-12.34e-6" ~: octLiteral "-12.34e-6" @?=
               -(167 % 4194304),
    "123.456e-6" ~: octLiteral "123.456e-6" @?=
               21399 % 67108864,
    "+123.456e-6" ~: octLiteral "+123.456e-6" @?=
                21399 % 67108864,
    "-123.456e-6" ~: octLiteral "-123.456e-6" @?=
                -21399 % 67108864,
    "1234.567e-6" ~: octLiteral "1234.567e-6" @?=
                342391 % 134217728,
    "+1234.567e-6" ~: octLiteral "+1234.567e-6" @?=
                342391 % 134217728,
    "-1234.567e-6" ~: octLiteral "-1234.567e-6" @?=
                 -342391 % 134217728
    ]

testBinLiteral = [
    "0" ~: binLiteral "0" @?= 0x0,
    "+0" ~: binLiteral "+0" @?= 0x0,
    "-0" ~: binLiteral "-0" @?= 0x0,
    "1" ~: binLiteral "1" @?= 0x1,
    "+1" ~: binLiteral "+1" @?= 0x1,
    "-1" ~: binLiteral "-1" @?= -0x1,
    "101" ~: binLiteral "101" @?= 0x5,
    "+101" ~: binLiteral "+101" @?= 0x5,
    "-101" ~: binLiteral "-101" @?= -0x5,
    "1011" ~: binLiteral "1011" @?= 0xb,
    "+1011" ~: binLiteral "+1011" @?= 0xb,
    "-1011" ~: binLiteral "-1011" @?= -0xb,
    "1101011" ~: binLiteral "1101011" @?= 0x6b,
    "+1101011" ~: binLiteral "+1101011" @?= 0x6b,
    "-1101011" ~: binLiteral "-1101011" @?= -0x6b,
    "10110101101" ~: binLiteral "10110101101" @?= 0x5ad,
    "+10110101101" ~: binLiteral "+10110101101" @?= 0x5ad,
    "-10110101101" ~: binLiteral "-10110101101" @?= -0x5ad,
    "0.0" ~: binLiteral "0.0" @?=
             0x0,
    "+0.0" ~: binLiteral "+0.0" @?=
              0x0,
    "-0.0" ~: binLiteral "-0.0" @?=
              0x0,
    "0.1" ~: binLiteral "0.1" @?=
             0x1 % 0x2,
    "+0.1" ~: binLiteral "+0.1" @?=
              0x1 % 0x2,
    "-0.1" ~: binLiteral "-0.1" @?=
              -(0x1 % 0x2),
    "0.101" ~: binLiteral "0.101" @?=
              0x5 % 0x8,
    "+0.101" ~: binLiteral "+0.101" @?=
               0x5 % 0x8,
    "-0.101" ~: binLiteral "-0.101" @?=
               -(0x5 % 0x8),
    "0.1011" ~: binLiteral "0.1011" @?=
               0xb % 0x10,
    "+0.1011" ~: binLiteral "+0.1011" @?=
                0xb % 0x10,
    "-0.1011" ~: binLiteral "-0.1011" @?=
                -0xb % 0x10,
    "0.1101011" ~: binLiteral "0.1101011" @?=
                0x6b % 0x80,
    "+0.1101011" ~: binLiteral "+0.1101011" @?=
                 0x6b % 0x80,
    "-0.1101011" ~: binLiteral "-0.1101011" @?=
                 -0x6b % 0x80,
    "0.10110101101" ~: binLiteral "0.10110101101" @?=
                 0x5ad % 0x800,
    "+0.10110101101" ~: binLiteral "+0.10110101101" @?=
                  0x5ad % 0x800,
    "-0.10110101101" ~: binLiteral "-0.10110101101" @?=
                  -(0x5ad % 0x800),
    "10.01" ~: binLiteral "10.01" @?=
             0x9 % 0x4,
    "+10.01" ~: binLiteral "+10.01" @?=
              0x9 % 0x4,
    "-10.01" ~: binLiteral "-10.01" @?=
              -(0x9 % 0x4),
    "101.011" ~: binLiteral "101.011" @?=
              0x2b % 0x8,
    "+101.011" ~: binLiteral "+101.011" @?=
               0x2b % 0x8,
    "-101.011" ~: binLiteral "-101.011" @?=
               -(0x2b % 0x8),
    "1001.0111" ~: binLiteral "1001.0111" @?=
               0x97 % 0x10,
    "+1001.0111" ~: binLiteral "+1001.0111" @?=
                0x97 % 0x10,
    "-1001.0111" ~: binLiteral "-1001.0111" @?=
                -0x97 % 0x10,
    "11000011.00011111" ~: binLiteral "11000011.00011111" @?=
                0xc31f % 0x100,
    "+11000011.00011111" ~: binLiteral "+11000011.00011111" @?=
                0xc31f % 0x100,
    "-11000011.00011111" ~: binLiteral "-11000011.00011111" @?=
                -0xc31f % 0x100,
    "10.01p1" ~: binLiteral "10.01p1" @?=
             0x9 % 0x2,
    "+10.01p1" ~: binLiteral "+10.01p1" @?=
             0x9 % 0x2,
    "-10.01p1" ~: binLiteral "-10.01p1" @?=
             -0x9 % 0x2,
    "101.011p1" ~: binLiteral "101.011p1" @?=
              0x2b % 0x4,
    "+101.011p1" ~: binLiteral "+101.011p1" @?=
              0x2b % 0x4,
    "-101.011p1" ~: binLiteral "-101.011p1" @?=
              -0x2b % 0x4,
    "1001.0111p1" ~: binLiteral "1001.0111p1" @?=
               0x97 % 0x8,
    "+1001.0111p1" ~: binLiteral "+1001.0111p1" @?=
               0x97 % 0x8,
    "-1001.0111p1" ~: binLiteral "-1001.0111p1" @?=
               -0x97 % 0x8,
    "11000011.00011111p1" ~: binLiteral "11000011.00011111p1" @?=
                0xc31f % 0x80,
    "+11000011.00011111p1" ~: binLiteral "+11000011.00011111p1" @?=
                0xc31f % 0x80,
    "-11000011.00011111p1" ~: binLiteral "-11000011.00011111p1" @?=
                -0xc31f % 0x80,
    "10.01p-1" ~: binLiteral "10.01p-1" @?=
             0x9 % 0x8,
    "+10.01p-1" ~: binLiteral "+10.01p-1" @?=
             0x9 % 0x8,
    "-10.01p-1" ~: binLiteral "-10.01p-1" @?=
             -0x9 % 0x8,
    "101.011p-1" ~: binLiteral "101.011p-1" @?=
              0x2b % 0x10,
    "+101.011p-1" ~: binLiteral "+101.011p-1" @?=
              0x2b % 0x10,
    "-101.011p-1" ~: binLiteral "-101.011p-1" @?=
              -0x2b % 0x10,
    "1001.0111p-1" ~: binLiteral "1001.0111p-1" @?=
               0x97 % 0x20,
    "+1001.0111p-1" ~: binLiteral "+1001.0111p-1" @?=
               0x97 % 0x20,
    "-1001.0111p-1" ~: binLiteral "-1001.0111p-1" @?=
               -0x97 % 0x20,
    "11000011.00011111p-1" ~: binLiteral "11000011.00011111p-1" @?=
                0xc31f % 0x200,
    "+11000011.00011111p-1" ~: binLiteral "+11000011.00011111p-1" @?=
                0xc31f % 0x200,
    "-11000011.00011111p-1" ~: binLiteral "-11000011.00011111p-1" @?=
                -0xc31f % 0x200,
    "1001p101" ~: binLiteral "1001p101" @?=
             0x120,
    "+1001p101" ~: binLiteral "+1001p101" @?=
             0x120,
    "-1001p101" ~: binLiteral "-1001p101" @?=
             -0x120,
    "101011p101" ~: binLiteral "101011p101" @?=
              0x560,
    "+101011p101" ~: binLiteral "+101011p101" @?=
              0x560,
    "-101011p101" ~: binLiteral "-101011p101" @?=
              -0x560,
    "10010111p101" ~: binLiteral "10010111p101" @?=
               0x12e0,
    "+10010111p101" ~: binLiteral "+10010111p101" @?=
               0x12e0,
    "-10010111p101" ~: binLiteral "-10010111p101" @?=
               -0x12e0,
    "1100001100011111p101" ~: binLiteral "1100001100011111p101" @?=
                0x1863e0,
    "+1100001100011111p101" ~: binLiteral "+1100001100011111p101" @?=
                0x1863e0,
    "-1100001100011111p101" ~: binLiteral "-1100001100011111p101" @?=
                -0x1863e0,
    "1001p-101" ~: binLiteral "1001p-101" @?=
             0x9 % 0x20,
    "+1001p-101" ~: binLiteral "+1001p-101" @?=
             0x9 % 0x20,
    "-1001p-101" ~: binLiteral "-1001p-101" @?=
             -0x9 % 0x20,
    "101011p-101" ~: binLiteral "101011p-101" @?=
              0x2b % 0x20,
    "+101011p-101" ~: binLiteral "+101011p-101" @?=
              0x2b % 0x20,
    "-101011p-101" ~: binLiteral "-101011p-101" @?=
              -0x2b % 0x20,
    "10010111p-101" ~: binLiteral "10010111p-101" @?=
               0x97 % 0x20,
    "+10010111p-101" ~: binLiteral "+10010111p-101" @?=
               0x97 % 0x20,
    "-10010111p-101" ~: binLiteral "-10010111p-101" @?=
               -0x97 % 0x20,
    "1100001100011111p-101" ~: binLiteral "1100001100011111p-101" @?=
                0xc31f % 0x20,
    "+1100001100011111p-101" ~: binLiteral "+1100001100011111p-101" @?=
                0xc31f % 0x20,
    "-1100001100011111p-101" ~: binLiteral "-1100001100011111p-101" @?=
                -0xc31f % 0x20,
    "10.01p101" ~: binLiteral "10.01p101" @?=
             0x48,
    "+10.01p101" ~: binLiteral "+10.01p101" @?=
             0x48,
    "-10.01p101" ~: binLiteral "-10.01p101" @?=
             -0x48,
    "101.011p101" ~: binLiteral "101.011p101" @?=
              0xac,
    "+101.011p101" ~: binLiteral "+101.011p101" @?=
              0xac,
    "-101.011p101" ~: binLiteral "-101.011p101" @?=
              -0xac,
    "1001.0111p101" ~: binLiteral "1001.0111p101" @?=
               0x12e,
    "+1001.0111p101" ~: binLiteral "+1001.0111p101" @?=
               0x12e,
    "-1001.0111p101" ~: binLiteral "-1001.0111p101" @?=
               -0x12e,
    "11000011.00011111p101" ~: binLiteral "11000011.00011111p101" @?=
                0xc31f % 0x8,
    "+11000011.00011111p101" ~: binLiteral "+11000011.00011111p101" @?=
                0xc31f % 0x8,
    "-11000011.00011111p101" ~: binLiteral "-11000011.00011111p101" @?=
                -0xc31f % 0x8,
    "10.01p-101" ~: binLiteral "10.01p-101" @?=
             0x9 % 0x80,
    "+10.01p-101" ~: binLiteral "+10.01p-101" @?=
             0x9 % 0x80,
    "-10.01p-101" ~: binLiteral "-10.01p-101" @?=
             -0x9 % 0x80,
    "101.011p-101" ~: binLiteral "101.011p-101" @?=
              0x2b % 0x100,
    "+101.011p-101" ~: binLiteral "+101.011p-101" @?=
              0x2b % 0x100,
    "-101.011p-101" ~: binLiteral "-101.011p-101" @?=
              -0x2b % 0x100,
    "1001.0111p-101" ~: binLiteral "1001.0111p-101" @?=
               0x97 % 0x200,
    "+1001.0111p-101" ~: binLiteral "+1001.0111p-101" @?=
               0x97 % 0x200,
    "-1001.0111p-101" ~: binLiteral "-1001.0111p-101" @?=
               -0x97 % 0x200,
    "11000011.00011111p-101" ~: binLiteral "11000011.00011111p-101" @?=
                0xc31f % 0x2000,
    "+11000011.00011111p-101" ~: binLiteral "+11000011.00011111p-101" @?=
                0xc31f % 0x2000,
    "-11000011.00011111p-101" ~: binLiteral "-11000011.00011111p-101" @?=
                -0xc31f % 0x2000
    ]

testlist :: [Test]
testlist = [
    "hexDigit" ~: testHexDigit,
    "decDigit" ~: testDecDigit,
    "octDigit" ~: testOctDigit,
    "binDigit" ~: testBinDigit,
    "hexNatural" ~: testHexNatural,
    "decNatural" ~: testDecNatural,
    "octNatural" ~: testOctNatural,
    "binNatural" ~: testBinNatural,
    "hexInteger" ~: testHexInteger,
    "decInteger" ~: testDecInteger,
    "octInteger" ~: testOctInteger,
    "binInteger" ~: testBinInteger,
    "hexRational" ~: testHexRational,
    "decRational" ~: testDecRational,
    "octRational" ~: testOctRational,
    "binRational" ~: testBinRational,
    "hexLiteral" ~: testHexLiteral,
    "decLiteral" ~: testDecLiteral,
    "octLiteral" ~: testOctLiteral,
    "binLiteral" ~: testBinLiteral
  ]

tests :: Test
tests = "Numbers" ~: testlist
