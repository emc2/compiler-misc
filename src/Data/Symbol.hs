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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- | This module provides an implementation of a very common technique
-- in compiler implementation.  Names are assigned to a unique number
-- during lexing, which allows them to be compared with a simple
-- numerical equality check thereafter.  This also allows symbols to
-- be stored in arrays as opposed to hash maps.
module Data.Symbol(
       Symbol,
       -- * Accessors
       symbol,
       ) where

import Data.Hashable
import Data.Ix
import Data.Word

-- | Symbol datatype.  Symbols are used as 
newtype Symbol =
  Symbol {
    -- | The unique numerical ID of the symbol.
    number :: Word
  }
  deriving (Eq, Ord, Ix)

-- | Create a symbol
symbol :: Word
       -- ^ Index value (this determines equality and ordering)
       -> Symbol
       -- ^ Symbol value
symbol n = Symbol { number = n }

instance Enum Symbol where
  succ = Symbol . succ . number
  pred = Symbol . pred . number
  toEnum = Symbol . toEnum
  fromEnum = fromEnum . number
  enumFromThen Symbol { number = n } = map Symbol . enumFromThen n . number
  enumFromTo Symbol { number = n } = map Symbol . enumFromTo n . number
  enumFromThenTo Symbol { number = n } Symbol { number = m } =
    map Symbol . enumFromThenTo n m . number

instance Hashable Symbol where
  hashWithSalt s Symbol { number = n } = hashWithSalt s n
