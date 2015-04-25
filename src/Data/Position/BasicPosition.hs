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

import Data.Hashable
import Data.Position
import Data.Position.PositionInfo
import Data.Semigroup
import Data.Word
import Text.Format hiding (line)
import Text.FormatM hiding (line)
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

import qualified Data.ByteString as Strict

-- | Information about a file.
data FileInfo =
  FileInfo {
    -- | The (relative) file name.
    fileInfoName :: !Strict.ByteString,
    -- | The base directory.
    fileInfoDir :: !Strict.ByteString
  }
  deriving (Ord, Eq)

-- | A specific line and column in a source file.
data PositionInfo =
  PositionInfo {
      -- | The line number, starting at 1.
      posLine :: !Word,
      -- | The column number, staring at 1.
      posColumn :: !Word
    }

data BasicPosition =
  -- | A span in a source file.
    Span {
      -- | The name of the source file.
      spanFile :: !Filename,
      -- | The starting point.
      spanStart :: !Position,
      -- | The starting point.
      spanEnd :: !Position
    }
    -- | A specific line and column in a source file.
  | Point {
      -- | The name of the source file.
      pointFile :: !Filename,
      -- | The position.
      pointPos :: !Position
    }
    -- | A position representing a whole file.
  | File {
      -- | The name of the source file.
      fileName :: !Filename
    }
    -- | A synthetic position, generated internally by a compiler.
  | Synthetic {
      -- | A description of the origin of this position.
      synthDesc :: !Strict.ByteString
    }
    -- | A command-line option.
  | CmdLine
  deriving (Ord, Eq)

instance Semigroup BasicPosition where
  Point { pointFile = fname1, pointPos = pos1 } <>
    Point { pointFile = fname2, pointPos = pos2 }
    | fname1 == fname2 = Span { spanStart = pos1, spanEnd = pos2,
                                spanFile = fname1 }
    | otherwise = error "Cannot combine positions in different files"
  Span { spanFile = fname1, spanStart = pos1 } <>
    Point { pointFile = fname2, pointPos = pos2 }
    | fname1 == fname2 = Span { spanStart = pos1, spanEnd = pos2,
                                spanFile = fname1 }
    | otherwise = error "Cannot combine positions in different files"
  Point { pointFile = fname1, pointPos = pos1 } <>
    Span { spanFile = fname2, spanEnd = pos2 }
    | fname1 == fname2 = Span { spanStart = pos1, spanEnd = pos2,
                                spanFile = fname1 }
    | otherwise = error "Cannot combine positions in different files"
  Span { spanFile = fname1, spanStart = pos1 } <>
    Span { spanFile = fname2, spanEnd = pos2 }
    | fname1 == fname2 = Span { spanStart = pos1, spanEnd = pos2,
                                spanFile = fname1 }
    | otherwise = error "Cannot combine positions in different files"
  CmdLine <> CmdLine = CmdLine
  _ <> _ = error $! "Cannot combine positions"

instance Format FileInfo where
  format FileInfo { fileInfoName = fname, fileInfoDir = dir } =
    cat [bytestring dir, bytestring fname]

instance Format PositionInfo where
  format PositionInfo { posLine = line, posColumn = col } =
    hcat [ format line, char '.', format col ]

instance FormatM BasicPosition where
  formatM Span { spanFile = fname, spanStart = start, spanEnd = end }
    | start == end =
      do
        posinfo <- positionInfo start
        return $! hcat [ format fileinfo, colon, format posinfo ]
    | start == end =
      do
        PositionInfo { posLine = startline, posColumn = startcol } <-
          positionInfo start
        PositionInfo { posLine = endline, posColumn = endcol } <-
          positionInfo end
        return $! hcat [ format fileinfo, comma,
                         format startline, dot, format startcol, char '-',
                         format endline, dot format endcol ]

instance Show PositionInfo where
  show = show . renderOneLine . format

instance Hashable FileInfo where
  hashWithSalt s FileInfo { fileInfoName = fname, fileInfoDir = dir } =
    s `hashWithSalt` fname `hashWithSalt` dir

instance Hashable PositionInfo where
  hashWithSalt s PositionInfo { posLine = line, posColumn = col } =
    s `hashWithSalt` line `hashWithSalt` col

instance Hashable BasicPosition where
  hashWithSalt s Span { spanStart = start, spanEnd = end,
                        spanFile = fname } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt`
    fname `hashWithSalt` start `hashWithSalt` end
  hashWithSalt s Point { pointPos = pos, pointFile = fname } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` pos  `hashWithSalt` fname
  hashWithSalt s File { fileName = fname } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` fname
  hashWithSalt s Synthetic { synthDesc = desc } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` desc
  hashWithSalt s CmdLine = s `hashWithSalt` (4 :: Int)

spanPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] BasicPosition
spanPickler =
  let
    fwdfunc ((), (fname, start, end)) =
      Span { spanStart = start, spanEnd = end, spanFile = fname }

    revfunc Span { spanStart = start, spanEnd = end, spanFile = fname } =
      ((), (fname, start, end))
    revfunc _ = error $! "Can't convert to Span"
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpAttrFixed (gxFromString "kind") (gxFromString "Span"))
                   (xpTriple (xpElemAttrs (gxFromString "file") xpickle)
                             (xpElemAttrs (gxFromString "start") xpickle)
                             (xpElemAttrs (gxFromString "pos") xpickle)))

pointPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] BasicPosition
pointPickler =
  let
    fwdfunc ((), (fname, pos)) = Point { pointFile = fname, pointPos = pos }

    revfunc Point { pointFile = fname, pointPos = pos } = ((), (fname, pos))
    revfunc _ = error $! "Can't convert to Point"
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpAttrFixed (gxFromString "kind") (gxFromString "Point"))
                   (xpPair (xpElemAttrs (gxFromString "file") xpickle)
                           (xpElemAttrs (gxFromString "pos") xpickle)))

eofPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] BasicPosition
eofPickler =
  let
    revfunc File { fileName = fname } = ((), fname)
    revfunc _ = error $! "Can't convert to File"
  in
    xpWrap (File . snd, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpAttrFixed (gxFromString "kind")
                                (gxFromString "EndOfFile"))
                   (xpElemAttrs (gxFromString "file") xpickle))

syntheticPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] BasicPosition
syntheticPickler =
  let
    revfunc Synthetic { synthDesc = desc } = ((), desc)
    revfunc _ = error $! "Can't convert Synthetic"
  in
    xpWrap (Synthetic . snd, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpAttrFixed (gxFromString "kind")
                                (gxFromString "Synthetic"))
                   (xpElemNodes (gxFromString "description")
                                (xpContent xpPrim)))

cmdLinePickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] BasicPosition
cmdLinePickler =
  let
    revfunc CmdLine = ()
    revfunc _ = error $! "Can't convert to CmdArg"
  in
    xpWrap (const CmdLine, revfunc)
           (xpElemAttrs (gxFromString "PositionInfo")
                        (xpAttrFixed (gxFromString "kind")
                                     (gxFromString "CmdLine")))

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
      xpAlt picker [spanPickler, pointPickler, eofPickler,
                    syntheticPickler, cmdLinePickler]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) PositionInfo where
  xpickle = xpWrap (\(line, col) ->
                     PositionInfo { posLine = line, posColumn = col },
                    \PositionInfo { posLine = line, posColumn = col } ->
                     (line, col))
                   (xpPair (xpAttr (gxFromString "line") xpPrim)
                           (xpAttr (gxFromString "col") xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) FileInfo where
  xpickle = xpWrap (\(fname, dir) ->
                     FileInfo { fileInfoName = gxToByteString fname,
                                fileInfoDir = gxToByteString dir },
                    \FileInfo { fileInfoName = fname,
                                fileInfoDir = dir } ->
                     (gxFromByteString fname, gxFromByteString dir))
                   (xpPair (xpAttr (gxFromString "name") xpText)
                           (xpAttr (gxFromString "dir") xpText))
