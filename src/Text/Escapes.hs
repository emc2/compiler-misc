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

-- | Facilities for parsing escape sequences.  Functions defined in
-- this file interpret escapes based on the C/C++ standard, expanded
-- to accomodate Unicode characters.
module Text.Escapes(
       fromEscapeChar,
       fromEscape
       ) where

import Data.Char
import Text.Numbers

-- | Map the character in a single-character escape to the escaped
-- character if preceeded by a backslash.
--
-- Example: the character @'n'@ is mapped to the character @'\n'@.
fromEscapeChar :: Char -> Char
fromEscapeChar 'a' = '\a'
fromEscapeChar 'b' = '\b'
fromEscapeChar 'f' = '\f'
fromEscapeChar 'n' = '\n'
fromEscapeChar 'r' = '\r'
fromEscapeChar 't' = '\t'
fromEscapeChar 'v' = '\v'
fromEscapeChar '\\' = '\\'
fromEscapeChar '\'' = '\''
fromEscapeChar '\"' = '\"'
fromEscapeChar '?' = '?'
fromEscapeChar c = error $! "Unrecognized escape sequence \\" ++ [c]

-- | Translate an escape sequence into the escaped character it
-- represents.
fromEscape :: String -> Char
fromEscape ('\\' : 'x' : hexstr) = chr (fromInteger (hexNatural hexstr))
fromEscape ('\\' : '0' : octstr) = chr (fromInteger (octNatural octstr))
fromEscape ('\\' : decstr @ (num : _)) | isDigit num =
  chr (fromInteger (decNatural decstr))
fromEscape ('\\' : 'b' : binstr) = chr (fromInteger (binNatural binstr))
fromEscape ['\\', char] = fromEscapeChar char
fromEscape s = error $! "Bad escape sequence " ++ s
