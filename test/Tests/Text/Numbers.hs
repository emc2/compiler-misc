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
    "binRational" ~: testBinRational
  ]

tests :: Test
tests = "Numbers" ~: testlist
