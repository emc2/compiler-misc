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

module Data.Position(
       -- * Types
       Position,

       firstPosition,
       debugStr
       ) where

import Data.Hashable
import Data.Ix
import Data.Word

-- | Symbol datatype.  Symbols are used as
newtype Position =
  Position {
    -- | The unique numerical ID of the symbol.
    idx :: Word
  }
  deriving (Eq, Ord, Ix)

-- | A starting point for enumerating symbols.
firstPosition :: Position
firstPosition = Position { idx = 0 }

-- | Get a debugging name for a Symbol
debugStr :: Position -> String
debugStr Position { idx = n } = "<position " ++ show n ++ ">"

instance Enum Position where
  succ = Position . succ . idx
  pred = Position . pred . idx
  toEnum = Position . toEnum
  fromEnum = fromEnum . idx
  enumFromThen Position { idx = n } = map Position . enumFromThen n . idx
  enumFromTo Position { idx = n } = map Position . enumFromTo n . idx
  enumFromThenTo Position { idx = n } Position { idx = m } =
    map Position . enumFromThenTo n m . idx

instance Hashable Position where
  hashWithSalt s Position { idx = n } = hashWithSalt s n
