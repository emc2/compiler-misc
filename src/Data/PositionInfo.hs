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

-- | Defines datatype for information about source element positions.
module Data.PositionInfo(
       PositionInfo(..)
       ) where

import Data.ByteString
import Data.Hashable
import Data.Word
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

-- | Datatype for information about source element positions.
data PositionInfo =
    -- | A span in a source file.
    Span {
      -- | The name of the source file.
      spanFile :: !ByteString,
      -- | The starting line number, starting at 1.
      spanStartLine :: !Word,
      -- | The starting column number, staring at 1.
      spanStartColumn :: !Word,
      -- | The ending line number, starting at 1.
      spanEndLine :: !Word,
      -- | The ending column number, staring at 1.
      spanEndColumn :: !Word
    }
    -- | A specific line and column in a source file.
  | Point {
      -- | The name of the source file.
      pointFile :: !ByteString,
      -- | The line number, starting at 1.
      pointLine :: !Word,
      -- | The column number, staring at 1.
      pointColumn :: !Word
    }
    -- | A position representing the end of input for a file.
  | EndOfFile {
      -- | The name of the source file.
      eofFile :: !ByteString
    }
    -- | A synthetic position, generated internally by a compiler.
  | Synthetic {
      -- | A description of the origin of this position.
      synthDesc :: !ByteString
    }
  deriving (Ord, Eq)

instance Show PositionInfo where
  show Span { spanStartLine = startline, spanStartColumn = startcol,
              spanEndLine = endline, spanEndColumn = endcol,
              spanFile = fname }
    | startline == endline = show fname ++ ":" ++ show startline ++ "." ++
                             show startcol ++ "-" ++ show endcol
    | otherwise = show fname ++ ":" ++ show startline ++ "." ++ show startcol ++
                  "-" ++ show endline ++ "." ++ show endcol
  show Point { pointLine = line, pointColumn = col, pointFile = fname } =
    show fname ++ ":" ++ show line ++ "." ++ show col
  show EndOfFile { eofFile = fname } = show fname ++ ": end of input"
  show Synthetic { synthDesc = desc } = show desc

instance Hashable PositionInfo where
  hashWithSalt s Span { spanStartLine = startline, spanEndLine = endline,
                        spanStartColumn = startcol, spanEndColumn = endcol,
                        spanFile = fname } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` fname `hashWithSalt`
    startline `hashWithSalt` startcol `hashWithSalt`
    endline `hashWithSalt` endcol
  hashWithSalt s Point { pointLine = line, pointColumn = col,
                         pointFile = fname } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt`
    line `hashWithSalt` col `hashWithSalt` fname
  hashWithSalt s EndOfFile { eofFile = fname } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` fname
  hashWithSalt s Synthetic { synthDesc = desc } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` desc

spanPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] PositionInfo
spanPickler =
  let
    fwdfunc ((), (startline, startcol, endline, endcol, fname)) =
      Span { spanStartLine = startline, spanStartColumn = startcol,
             spanEndLine = endline, spanEndColumn = endcol,
             spanFile = fname }

    revfunc Span { spanStartLine = startline, spanStartColumn = startcol,
                   spanEndLine = endline, spanEndColumn = endcol,
                   spanFile = fname} =
      ((), (startline, startcol, endline, endcol, fname))
    revfunc pinfo = error $! "Can't convert " ++ show pinfo
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpAttrFixed (gxFromString "kind")
                                (gxFromString "Span"))
                   (xp5Tuple (xpElemNodes (gxFromString "start-line")
                                          (xpContent xpPrim))
                             (xpElemNodes (gxFromString "start-column")
                                          (xpContent xpPrim))
                             (xpElemNodes (gxFromString "end-line")
                                          (xpContent xpPrim))
                             (xpElemNodes (gxFromString "end-column")
                                          (xpContent xpPrim))
                             (xpElemNodes (gxFromString "file")
                                          (xpContent xpPrim))))

pointPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] PositionInfo
pointPickler =
  let
    fwdfunc ((), (line, col, fname)) =
      Point { pointFile = fname, pointLine = line, pointColumn = col }

    revfunc Point { pointFile = fname, pointLine = line, pointColumn = col } =
      ((), (line, col, fname))
    revfunc pinfo = error $! "Can't convert " ++ show pinfo
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpAttrFixed (gxFromString "kind")
                                (gxFromString "Point"))
                   (xpTriple (xpElemNodes (gxFromString "line")
                                          (xpContent xpPrim))
                             (xpElemNodes (gxFromString "column")
                                          (xpContent xpPrim))
                             (xpElemNodes (gxFromString "file")
                                          (xpContent xpPrim))))

eofPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] PositionInfo
eofPickler =
  let
    revfunc EndOfFile { eofFile = fname } = ((), fname)
    revfunc pinfo = error $! "Can't convert " ++ show pinfo
  in
    xpWrap (EndOfFile . snd, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpAttrFixed (gxFromString "kind")
                                (gxFromString "EndOfFile"))
                   (xpElemNodes (gxFromString "file")
                                (xpContent xpPrim)))

syntheticPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] PositionInfo
syntheticPickler =
  let
    revfunc Synthetic { synthDesc = desc } = ((), desc)
    revfunc pinfo = error $! "Can't convert " ++ show pinfo
  in
    xpWrap (Synthetic . snd, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpAttrFixed (gxFromString "kind")
                                (gxFromString "Synthetic"))
                   (xpElemNodes (gxFromString "description")
                                (xpContent xpPrim)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] PositionInfo where
  xpickle =
    let
      picker Span {} = 0
      picker Point {} = 1
      picker EndOfFile {} = 2
      picker Synthetic {} = 3
    in
      xpAlt picker [spanPickler, pointPickler, eofPickler, syntheticPickler]
