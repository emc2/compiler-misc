-- Copyright (c) 2015 Eric McCorkle.  All rights reserved.
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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.PositionElement(
       PositionElement(..),
       DWARFPositionElement(..)
       ) where

import Data.Position.BasicPosition(BasicPosition)
import Data.Position.DWARFPosition(DWARFPosition)

import qualified Data.Position.BasicPosition as Basic
import qualified Data.Position.DWARFPosition as DWARF

-- | Typeclass of data with a notion of position.
class PositionElement ty where
  -- | Get position data.
  position :: ty -> BasicPosition

class PositionElement ty => DWARFPositionElement def tydef ty where
  -- | Get full debugging position data.
  debugPosition :: ty -> DWARFPosition def tydef
  debugPosition e =
    case position e of
      Basic.Span { Basic.spanStart = start, Basic.spanEnd = end } ->
        DWARF.Simple { DWARF.simplePos = DWARF.Span { DWARF.spanStart = start,
                                                      DWARF.spanEnd = end } }
      Basic.Point { Basic.pointPos = pos } ->
        DWARF.Simple { DWARF.simplePos = DWARF.Point { DWARF.pointPos = pos } }
      Basic.File { Basic.fileName = fname } ->
        DWARF.File { DWARF.fileName = fname }
      Basic.Synthetic { Basic.synthDesc = desc } ->
        DWARF.Synthetic { DWARF.synthDesc = desc }
      Basic.CmdLine -> DWARF.CmdLine
