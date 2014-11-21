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

-- | Provides parsing of number literals.  This module is analogous to
-- "Text.Numbers", except it works on 'ByteString's.
module Text.Numbers.ByteString(
       -- * Character Parsing
       hexDigit,
       decDigit,
       octDigit,
       binDigit,

       -- * ByteString Parsing
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

import Data.ByteString.UTF8

import qualified Text.Numbers as Numbers

-- | Convert a character representing a hex digit to an integer value.
hexDigit :: Char -> Integer
hexDigit = Numbers.hexDigit

-- | Convert a character representing a decimal digit to an integer value.
decDigit :: Char -> Integer
decDigit = Numbers.decDigit

-- | Convert a character representing an octal digit to an integer value.
octDigit :: Char -> Integer
octDigit = Numbers.octDigit

-- | Convert a character representing a binary digit to an integer value.
binDigit :: Char -> Integer
binDigit = Numbers.binDigit

-- | Parse a string representing a natural number in hex.  Does not
-- include the typical @0x@ header.
hexNatural :: ByteString -> Integer
hexNatural = Numbers.hexNatural . toString

-- | Parse a string representing a natural number in hex.  Does not
-- include the typical @0x@ header.
decNatural :: ByteString -> Integer
decNatural = Numbers.decNatural . toString

-- | Parse a string representing a natural number in octal.  Does not
-- include the typical @0@ header.
octNatural :: ByteString -> Integer
octNatural = Numbers.octNatural . toString

-- | Parse a string representing a natural number in binary.  Does not
-- include the typical @0b@ header.
binNatural :: ByteString -> Integer
binNatural = Numbers.binNatural . toString

-- | Parse a string representing an integer in hex, possibly with a
-- leading @+@ or @-@ indicating the sign.  If no sign is given, the
-- number is positive.
hexInteger :: ByteString -> Integer
hexInteger = Numbers.hexInteger . toString

-- | Parse a string representing an integer in decimal, possibly with
-- a leading @+@ or @-@ indicating the sign.  If no sign is given, the
-- number is positive.
decInteger :: ByteString -> Integer
decInteger = Numbers.decInteger . toString

-- | Parse a string representing an integer in octal, possibly with a
-- leading @+@ or @-@ indicating the sign.  If no sign is given, the
-- number is positive.
octInteger :: ByteString -> Integer
octInteger = Numbers.octInteger . toString

-- | Parse a string representing an integer in binary, possibly with a
-- leading @+@ or @-@ indicating the sign.  If no sign is given, the
-- number is positive.
binInteger :: ByteString -> Integer
binInteger = Numbers.binInteger . toString

-- | Parse a string reprensenting a floating point number in
-- hexidecimal.  The result will be represented with a @Rational@.
-- The format is @<whole part>.<fraction part>@, with digits in
-- hexidecimal, and an optional leading @+@ or @-@.
hexRational :: ByteString -> Rational
hexRational = Numbers.hexRational . toString

-- | Parse a string reprensenting a floating point number in
-- decimal.  The result will be represented with a @Rational@.
-- The format is @<whole part>.<fraction part>@, with digits in
-- decimal, and an optional leading @+@ or @-@.
decRational :: ByteString -> Rational
decRational = Numbers.decRational . toString

-- | Parse a string reprensenting a floating point number in
-- octal.  The result will be represented with a @Rational@.
-- The format is @<whole part>.<fraction part>@, with digits in
-- octal, and an optional leading @+@ or @-@.
octRational :: ByteString -> Rational
octRational = Numbers.octRational . toString

-- | Parse a string reprensenting a floating point number in
-- binary.  The result will be represented with a @Rational@.
-- The format is @<whole part>.<fraction part>@, with digits in
-- binary, and an optional leading @+@ or @-@.
binRational :: ByteString -> Rational
binRational = Numbers.binRational . toString

-- | Parse a string representing a floating point number in hex, with
-- an optional exponent.  The result will be represented with a
-- @Rational@.  The format is @<whole part>.<fractional part>@, with
-- an optional leading @+@ or @-@, optionally followed by @p@ or @P@
-- and a whole number exponent with an optional leading @+@ or @-@.
hexLiteral :: ByteString -> Rational
hexLiteral = Numbers.hexLiteral . toString

-- | Parse a string representing a floating point number in decimal, with
-- an optional exponent.  The result will be represented with a
-- @Rational@.  The format is @<whole part>.<fractional part>@, with
-- an optional leading @+@ or @-@, optionally followed by @e@, @E@,
-- @p@, or @P@ and a whole number exponent with an optional leading
-- @+@ or @-@.
decLiteral :: ByteString -> Rational
decLiteral = Numbers.decLiteral . toString

-- | Parse a string representing a floating point number in octal, with
-- an optional exponent.  The result will be represented with a
-- @Rational@.  The format is @<whole part>.<fractional part>@, with
-- an optional leading @+@ or @-@, optionally followed by @e@, @E@,
-- @p@, or @P@ and a whole number exponent with an optional leading
-- @+@ or @-@.
octLiteral :: ByteString -> Rational
octLiteral = Numbers.octLiteral . toString

-- | Parse a string representing a floating point number in binary, with
-- an optional exponent.  The result will be represented with a
-- @Rational@.  The format is @<whole part>.<fractional part>@, with
-- an optional leading @+@ or @-@, optionally followed by @e@, @E@,
-- @p@, or @P@ and a whole number exponent with an optional leading
-- @+@ or @-@.
binLiteral :: ByteString -> Rational
binLiteral = Numbers.binLiteral . toString
