-- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
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
{-# OPTIONS_GHC -Wall -Werror #-}

-- | Provides parsing of number literals.
module Text.Numbers(
       -- * Character Parsing
       hexDigit,
       decDigit,
       octDigit,
       binDigit,

       -- * String Parsing
       -- ** Positive Literals
       hexNatural,
       decNatural,
       octNatural,
       binNatural,

       -- ** Integer Literals
       hexInteger,
       decInteger,
       octInteger,
       binInteger,

       -- ** Rational Literals
       hexRational,
       decRational,
       octRational,
       binRational,

       -- ** General Literals
       hexLiteral,
       decLiteral,
       octLiteral,
       binLiteral,
) where

import Data.Bits
import Data.List
import Data.Ratio hiding (numerator)

-- | Convert a character representing a hex digit to an integer value.
hexDigit :: Char -> Integer
hexDigit '0' = 0x0
hexDigit '1' = 0x1
hexDigit '2' = 0x2
hexDigit '3' = 0x3
hexDigit '4' = 0x4
hexDigit '5' = 0x5
hexDigit '6' = 0x6
hexDigit '7' = 0x7
hexDigit '8' = 0x8
hexDigit '9' = 0x9
hexDigit 'a' = 0xa
hexDigit 'A' = 0xa
hexDigit 'b' = 0xb
hexDigit 'B' = 0xb
hexDigit 'c' = 0xc
hexDigit 'C' = 0xc
hexDigit 'd' = 0xd
hexDigit 'D' = 0xd
hexDigit 'e' = 0xe
hexDigit 'E' = 0xe
hexDigit 'f' = 0xf
hexDigit 'F' = 0xf
hexDigit c = error $! ("Unexpected hex character " ++ [c])

-- | Convert a character representing a decimal digit to an integer value.
decDigit :: Char -> Integer
decDigit '0' = 0x0
decDigit '1' = 0x1
decDigit '2' = 0x2
decDigit '3' = 0x3
decDigit '4' = 0x4
decDigit '5' = 0x5
decDigit '6' = 0x6
decDigit '7' = 0x7
decDigit '8' = 0x8
decDigit '9' = 0x9
decDigit c = error $! ("Unexpected decimal character " ++ [c])

-- | Convert a character representing an octal digit to an integer value.
octDigit :: Char -> Integer
octDigit '0' = 0x0
octDigit '1' = 0x1
octDigit '2' = 0x2
octDigit '3' = 0x3
octDigit '4' = 0x4
octDigit '5' = 0x5
octDigit '6' = 0x6
octDigit '7' = 0x7
octDigit c = error $! ("Unexpected octal character " ++ [c])

-- | Convert a character representing a binary digit to an integer value.
binDigit :: Char -> Integer
binDigit '0' = 0x0
binDigit '1' = 0x1
binDigit c = error $! ("Unexpected binary character " ++ [c])

-- | Parse a string representing a natural number in hex.  Does not
-- include the typical @0x@ header.
hexNatural :: String -> Integer
hexNatural =
  let
    foldfun accum digit = ((accum `shiftL` 4) .|. hexDigit digit)
  in
    foldl foldfun 0

-- | Parse a string representing a natural number in hex.  Does not
-- include the typical @0x@ header.
decNatural :: String -> Integer
decNatural =
  let
    foldfun accum digit = ((accum * 10) + hexDigit digit)
  in
    foldl foldfun 0

-- | Parse a string representing a natural number in octal.  Does not
-- include the typical @0@ header.
octNatural :: String -> Integer
octNatural =
  let
    foldfun accum digit = ((accum `shiftL` 3) .|. octDigit digit)
  in
    foldl foldfun 0

-- | Parse a string representing a natural number in binary.  Does not
-- include the typical @0b@ header.
binNatural :: String -> Integer
binNatural =
  let
    foldfun accum digit = ((accum `shiftL` 1) .|. binDigit digit)
  in
    foldl foldfun 0

-- | Parse a string representing an integer in hex, possibly with a
-- leading @+@ or @-@ indicating the sign.  If no sign is given, the
-- number is positive.
hexInteger :: String -> Integer
hexInteger ('-' : str) = -(hexNatural str)
hexInteger ('+' : str) = hexNatural str
hexInteger str = hexNatural str

-- | Parse a string representing an integer in decimal, possibly with
-- a leading @+@ or @-@ indicating the sign.  If no sign is given, the
-- number is positive.
decInteger :: String -> Integer
decInteger ('-' : str) = -(decNatural str)
decInteger ('+' : str) = decNatural str
decInteger str = decNatural str

-- | Parse a string representing an integer in octal, possibly with a
-- leading @+@ or @-@ indicating the sign.  If no sign is given, the
-- number is positive.
octInteger :: String -> Integer
octInteger ('-' : str) = -(octNatural str)
octInteger ('+' : str) = octNatural str
octInteger str = octNatural str

-- | Parse a string representing an integer in binary, possibly with a
-- leading @+@ or @-@ indicating the sign.  If no sign is given, the
-- number is positive.
binInteger :: String -> Integer
binInteger ('-' : str) = -(binNatural str)
binInteger ('+' : str) = binNatural str
binInteger str = binNatural str

-- | Parse a string reprensenting a floating point number in
-- hexidecimal.  The result will be represented with a @Rational@.
-- The format is @<whole part>.<fraction part>@, with digits in
-- hexidecimal, and an optional leading @+@ or @-@.
hexRational :: String -> Rational
hexRational str =
  let
    (numstr, pos) =
      case str of
        '-' : rest -> (rest, False)
        '+' : rest -> (rest, True)
        _ -> (str, True)
  in case elemIndex '.' numstr of
    Just index ->
      let
        wholestr = take index numstr
        fracstr = drop (index + 1) numstr
        wholepart = hexNatural wholestr
        fracpart = hexNatural fracstr
        fracpower = length fracstr
        numerator = (wholepart `shiftL` (4 * fracpower)) .|. fracpart
        denomenator = 1 `shiftL` (4 * fracpower)
      in
        if pos
          then numerator % denomenator
          else -numerator % denomenator
    Nothing ->
      toRational (if pos then hexNatural numstr else -hexNatural numstr)

-- | Parse a string reprensenting a floating point number in
-- decimal.  The result will be represented with a @Rational@.
-- The format is @<whole part>.<fraction part>@, with digits in
-- decimal, and an optional leading @+@ or @-@.
decRational :: String -> Rational
decRational str =
  let
    (numstr, pos) =
      case str of
        '-' : rest -> (rest, False)
        '+' : rest -> (rest, True)
        _ -> (str, True)
  in case elemIndex '.' numstr of
    Just index ->
      let
        wholestr = take index numstr
        fracstr = drop (index + 1) numstr
        wholepart = decInteger wholestr
        fracpart = decNatural fracstr
        fracpower = length fracstr
        numerator = (wholepart  * (10 ^ fracpower)) + fracpart
        denomenator = 1 * (10 ^ fracpower)
      in
        if pos
          then numerator % denomenator
          else -numerator % denomenator
    Nothing ->
      toRational (if pos then decNatural numstr else -decNatural numstr)

-- | Parse a string reprensenting a floating point number in
-- octal.  The result will be represented with a @Rational@.
-- The format is @<whole part>.<fraction part>@, with digits in
-- octal, and an optional leading @+@ or @-@.
octRational :: String -> Rational
octRational str =
  let
    (numstr, pos) =
      case str of
        '-' : rest -> (rest, False)
        '+' : rest -> (rest, True)
        _ -> (str, True)
  in case elemIndex '.' numstr of
    Just index ->
      let
        wholestr = take index numstr
        fracstr = drop (index + 1) numstr
        wholepart = octNatural wholestr
        fracpart = octNatural fracstr
        fracpower = length fracstr
        numerator = (wholepart `shiftL` (3 * fracpower)) .|. fracpart
        denomenator = 1 `shiftL` (3 * fracpower)
      in
        if pos
          then numerator % denomenator
          else -numerator % denomenator
    Nothing ->
      toRational (if pos then octNatural numstr else -octNatural numstr)

-- | Parse a string reprensenting a floating point number in
-- binary.  The result will be represented with a @Rational@.
-- The format is @<whole part>.<fraction part>@, with digits in
-- binary, and an optional leading @+@ or @-@.
binRational :: String -> Rational
binRational str =
  let
    (numstr, pos) =
      case str of
        '-' : rest -> (rest, False)
        '+' : rest -> (rest, True)
        _ -> (str, True)
  in case elemIndex '.' numstr of
    Just index ->
      let
        wholestr = take index numstr
        fracstr = drop (index + 1) numstr
        wholepart = binNatural wholestr
        fracpart = binNatural fracstr
        fracpower = length fracstr
        numerator = (wholepart `shiftL` fracpower) .|. fracpart
        denomenator = 1 `shiftL` fracpower
      in
        if pos
          then numerator % denomenator
          else -numerator % denomenator
    Nothing ->
      toRational (if pos then binNatural numstr else -binNatural numstr)

-- | Parse a string representing a floating point number in hex, with
-- an optional exponent.  The result will be represented with a
-- @Rational@.  The format is @<whole part>.<fractional part>@, with
-- an optional leading @+@ or @-@, optionally followed by @p@ or @P@
-- and a whole number exponent with an optional leading @+@ or @-@.
hexLiteral :: String -> Rational
hexLiteral str =
  let
    (basestr, power) =
      case findIndex (\c -> c == 'p' || c == 'P') str of
        Just index ->
          (take index str, fromInteger (hexInteger (drop (index + 1) str)))
        Nothing -> (str, 1)
  in
    case elemIndex '.' basestr of
      Just index ->
        let
          wholestr = take index basestr
          fracstr = drop (index + 1) basestr
          wholepart = hexInteger wholestr
          fracpart = hexNatural fracstr
          fracpower = length fracstr + (abs power - 1)
          shiftamount = 4 * fracpower
          numerator = (wholepart `shiftL` shiftamount) .|. fracpart
          denomenator = 1 `shiftL` shiftamount
        in
          if power > 0
            then numerator % denomenator
            else denomenator % numerator
      Nothing ->
        if power > 0
          then toRational (hexNatural str ^ power)
          else 1 % (hexNatural str ^ (-power))

-- | Parse a string representing a floating point number in decimal, with
-- an optional exponent.  The result will be represented with a
-- @Rational@.  The format is @<whole part>.<fractional part>@, with
-- an optional leading @+@ or @-@, optionally followed by @e@, @E@,
-- @p@, or @P@ and a whole number exponent with an optional leading
-- @+@ or @-@.
decLiteral :: String -> Rational
decLiteral str =
  let
    (basestr, power) =
      case findIndex (\c -> c == 'p' || c == 'P' || c == 'e' || c == 'E') str of
        Just index ->
          (take index str, fromInteger (decInteger (drop (index + 1) str)))
        Nothing -> (str, 1)
  in
    case elemIndex '.' basestr of
      Just index ->
        let
          wholestr = take index basestr
          fracstr = drop (index + 1) basestr
          wholepart = decInteger wholestr
          fracpart = decNatural fracstr
          fracpower = length fracstr + (abs power - 1)
          numerator = (wholepart * (10 ^ fracpower)) + fracpart
          denomenator = 10 ^ fracpower
        in
          if power > 0
            then numerator % denomenator
            else denomenator % numerator
      Nothing ->
        if power > 0
          then toRational (decNatural str ^ power)
          else 1 % (decNatural str ^ (-power))

-- | Parse a string representing a floating point number in octal, with
-- an optional exponent.  The result will be represented with a
-- @Rational@.  The format is @<whole part>.<fractional part>@, with
-- an optional leading @+@ or @-@, optionally followed by @e@, @E@,
-- @p@, or @P@ and a whole number exponent with an optional leading
-- @+@ or @-@.
octLiteral :: String -> Rational
octLiteral str =
  let
    (basestr, power) =
      case findIndex (\c -> c == 'p' || c == 'P' || c == 'e' || c == 'E') str of
        Just index ->
          (take index str, fromInteger (octInteger (drop (index + 1) str)))
        Nothing -> (str, 1)
  in
    case elemIndex '.' basestr of
      Just index ->
        let
          wholestr = take index basestr
          fracstr = drop (index + 1) basestr
          wholepart = octInteger wholestr
          fracpart = octNatural fracstr
          fracpower = length fracstr + (abs power - 1)
          shiftamount = fracpower * 3
          numerator = (wholepart `shiftL` shiftamount) .|. fracpart
          denomenator = 1 `shiftL` shiftamount
        in
          if power > 0
            then numerator % denomenator
            else denomenator % numerator
      Nothing ->
        if power > 0
          then toRational (octNatural str ^ power)
          else 1 % (octNatural str ^ (-power))

-- | Parse a string representing a floating point number in binary, with
-- an optional exponent.  The result will be represented with a
-- @Rational@.  The format is @<whole part>.<fractional part>@, with
-- an optional leading @+@ or @-@, optionally followed by @e@, @E@,
-- @p@, or @P@ and a whole number exponent with an optional leading
-- @+@ or @-@.
binLiteral :: String -> Rational
binLiteral str =
  let
    (basestr, power) =
      case findIndex (\c -> c == 'p' || c == 'P' || c == 'e' || c == 'E') str of
        Just index ->
          (take index str, fromInteger (binInteger (drop (index + 1) str)))
        Nothing -> (str, 1)
  in
    case elemIndex '.' basestr of
      Just index ->
        let
          wholestr = take index basestr
          fracstr = drop (index + 1) basestr
          wholepart = binInteger wholestr
          fracpart = binNatural fracstr
          fracpower = length fracstr + (abs power - 1)
          numerator = (wholepart `shiftL` fracpower) .|. fracpart
          denomenator = 1 `shiftL` fracpower
        in
          if power > 0
            then numerator % denomenator
            else denomenator % numerator
      Nothing ->
        if power > 0
          then toRational (binNatural str ^ power)
          else 1 % (binNatural str ^ (-power))
