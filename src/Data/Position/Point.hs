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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Provides a type to use as a placeholder for a position.  These
-- are used to look up the actual position information in a position
-- database.
--
-- This is quite similar in concept to @Symbol@s
module Data.Position.Point(
       Point(..),
       PointInfo(..),
       firstPoint,
       ) where

import Data.Hashable
import Data.Ix
import Data.Position.Filename
import Text.Format hiding (line)
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

-- | This is a token used to represent a specific position in a file.
-- This can be used as a key to look up a 'PointInfo' inside a
-- 'Positions' monad instance.
newtype Point =
  Point {
    -- | The unique numerical ID of the symbol.
    pIdx :: Word
  }
  deriving (Eq, Ord, Ix)

-- | A specific line and column in a source file.
data PointInfo =
  PointInfo {
      -- | The file name associated with this point.
      pointFile :: !Filename,
      -- | The line number, starting at 1.
      pointLine :: !Word,
      -- | The column number, staring at 1.
      pointColumn :: !Word
    }
  deriving (Ord, Eq)

-- | A starting point for enumerating @Point@s.
firstPoint :: Point
firstPoint = Point { pIdx = 0 }

instance Enum Point where
  succ = Point . succ . pIdx
  pred = Point . pred . pIdx
  toEnum = Point . toEnum
  fromEnum = fromEnum . pIdx
  enumFromThen Point { pIdx = n } = map Point . enumFromThen n . pIdx
  enumFromTo Point { pIdx = n } = map Point . enumFromTo n . pIdx
  enumFromThenTo Point { pIdx = n } Point { pIdx = m } =
    map Point . enumFromThenTo n m . pIdx

instance Hashable Point where
  hashWithSalt s Point { pIdx = n } = hashWithSalt s n

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Point where
  xpickle = xpWrap (Point, pIdx) (xpElemNodes (gxFromString "Point")
                                                (xpContent xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Point where
  xpickle = xpWrap (Point, pIdx) (xpAttr (gxFromString "pos") xpPrim)

instance Format PointInfo where
  format PointInfo { pointLine = line, pointColumn = col } =
    hcat [ format line, char '.', format col ]

instance Show PointInfo where
  show = show . renderOneLine . format


instance Hashable PointInfo where
  hashWithSalt s PointInfo { pointLine = line, pointColumn = col } =
    s `hashWithSalt` line `hashWithSalt` col

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) PointInfo where
  xpickle = xpWrap (\(fname, line, col) ->
                     PointInfo { pointFile = fname, pointLine = line,
                                 pointColumn = col },
                    \PointInfo { pointFile = fname, pointLine = line,
                                 pointColumn = col } ->
                     (fname, line, col))
                   (xpTriple xpickle
                             (xpAttr (gxFromString "line") xpPrim)
                             (xpAttr (gxFromString "col") xpPrim))
