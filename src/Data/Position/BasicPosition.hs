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

-- | Defines datatype for information about source element positions.
module Data.Position.BasicPosition(
       BasicPosition(..)
       ) where

import Control.Monad.Positions
import Data.Hashable
import Data.Semigroup
import Data.Word
import Text.Format hiding (line)
import Text.FormatM hiding (line)
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict
import qualified Data.Position as Position

data BasicPosition =
  -- | A span in a source file.
    Span {
      -- | The starting point.
      spanStart :: !Position.Point,
      -- | The starting point.
      spanEnd :: !Position.Point
    }
    -- | A specific line and column in a source file.
  | Point {
      -- | The position.
      pointPos :: !Position.Point
    }
    -- | A position representing a whole file.
  | File {
      -- | The name of the source file.
      fileName :: !Position.Filename
    }
    -- | A synthetic position, generated internally by a compiler.
  | Synthetic {
      -- | A description of the origin of this position.
      synthDesc :: !Strict.ByteString
    }
    -- | A command-line option.
  | CmdLine
  deriving (Ord, Eq)

instance Position.PositionInfo BasicPosition where
  location Span { spanStart = startpos, spanEnd = endpos } =
    do
      Position.PointInfo { Position.pointFile = fname } <- pointInfo startpos
      return (Just (fname, Just (startpos, endpos)))
  location Point { pointPos = pos } =
    do
      Position.PointInfo { Position.pointFile = fname } <- pointInfo pos
      return (Just (fname, Just (pos, pos)))
  location File { fileName = fname } = return (Just (fname, Nothing))
  location _ = return Nothing

  description Synthetic { synthDesc = desc } = desc
  description CmdLine = Strict.fromString "from command line"
  description _ = Strict.empty

  children _ = Nothing

  showContext _ = True

instance Position.Position BasicPosition BasicPosition where
  positionInfo pos = [pos]

instance Semigroup BasicPosition where
  Point { pointPos = pos1 } <> Point { pointPos = pos2 } =
    Span { spanStart = pos1, spanEnd = pos2 }
  Span { spanStart = pos1 } <> Point { pointPos = pos2 } =
    Span { spanStart = pos1, spanEnd = pos2 }
  Point { pointPos = pos1 } <> Span { spanEnd = pos2 } =
    Span { spanStart = pos1, spanEnd = pos2 }
  Span { spanStart = pos1 } <> Span { spanEnd = pos2 } =
    Span { spanStart = pos1, spanEnd = pos2 }
  CmdLine <> CmdLine = CmdLine
  _ <> _ = error $! "Cannot combine positions"

instance Hashable BasicPosition where
  hashWithSalt s Span { spanStart = start, spanEnd = end } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` start `hashWithSalt` end
  hashWithSalt s Point { pointPos = pos } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` pos
  hashWithSalt s File { fileName = fname } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` fname
  hashWithSalt s Synthetic { synthDesc = desc } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` desc
  hashWithSalt s CmdLine = s `hashWithSalt` (4 :: Int)

instance (MonadPositions m) => FormatM m BasicPosition where
  formatM Span { spanStart = startpos, spanEnd = endpos } =
    do
      Position.PointInfo { Position.pointLine = startline,
                           Position.pointColumn = startcol,
                           Position.pointFile = fname } <- pointInfo startpos
      Position.PointInfo { Position.pointLine = endline,
                           Position.pointColumn = endcol } <- pointInfo endpos
      Position.FileInfo { Position.fileInfoName = fstr } <- fileInfo fname
      if startline == endline
        then return (hcat [bytestring fstr, colon, format startline, dot,
                           format startcol, char '-', format endcol])
        else return (hcat [bytestring fstr, colon,
                           format startline, dot, format startcol, char '-',
                           format endline, dot, format endcol])
  formatM Point { pointPos = pos } =
    do
      Position.PointInfo { Position.pointLine = line,
                           Position.pointColumn = col,
                           Position.pointFile = fname } <- pointInfo pos
      Position.FileInfo { Position.fileInfoName = fstr } <- fileInfo fname
      return (hcat [bytestring fstr, colon, format line, dot, format col])
  formatM File { fileName = fname } =
    do
      Position.FileInfo { Position.fileInfoName = fstr } <- fileInfo fname
      return (bytestring fstr)
  formatM CmdLine = return (string "command line")
  formatM Synthetic { synthDesc = desc } = return (bytestring desc)


spanPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] BasicPosition
spanPickler =
  let
    fwdfunc (start, end) =
      Span { spanStart = start, spanEnd = end }

    revfunc Span { spanStart = start, spanEnd = end } =
      (start, end)
    revfunc _ = error $! "Can't convert to Span"
  in
    xpWrap (fwdfunc, revfunc)
           (xpElemNodes (gxFromString "Span")
                        (xpPair (xpElemAttrs (gxFromString "start") xpickle)
                                (xpElemAttrs (gxFromString "end") xpickle)))

pointPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] BasicPosition
pointPickler =
  let
    revfunc Point { pointPos = pos } = pos
    revfunc _ = error $! "Can't convert to Point"
  in
    xpWrap (Point, revfunc) (xpElemAttrs (gxFromString "Point") xpickle)

filePickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] BasicPosition
filePickler =
  let
    revfunc File { fileName = fname } = fname
    revfunc _ = error $! "Can't convert to File"
  in
    xpWrap (File, revfunc) (xpElemAttrs (gxFromString "File") xpickle)

syntheticPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] BasicPosition
syntheticPickler =
  let
    revfunc Synthetic { synthDesc = desc } = gxFromByteString desc
    revfunc _ = error $! "Can't convert to Synthetic"
  in
    xpWrap (Synthetic . gxToByteString, revfunc)
           (xpElemNodes (gxFromString "Synthetic") (xpContent xpText))

cmdLinePickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] BasicPosition
cmdLinePickler =
  let
    revfunc CmdLine = ()
    revfunc _ = error $! "Can't convert to CmdArg"
  in
    xpWrap (const CmdLine, revfunc)
           (xpElemNodes (gxFromString "CmdLine") xpUnit)

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] BasicPosition where
  xpickle =
    let
      picker Span {} = 0
      picker Point {} = 1
      picker File {} = 2
      picker Synthetic {} = 3
      picker CmdLine {} = 4
    in
      xpAlt picker [spanPickler, pointPickler, filePickler,
                    syntheticPickler, cmdLinePickler]
