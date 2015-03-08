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
       PositionInfo(..),
       filepath,
       start,
       end
       ) where

import Data.ByteString.Char8
import Data.Hashable
import Data.Semigroup
import Data.Word
import Text.Format hiding (line)
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
    -- | A position representing a whole file.
  | File {
      -- | The name of the source file.
      fileName :: !ByteString
    }
    -- | A synthetic position, generated internally by a compiler.
  | Synthetic {
      -- | A description of the origin of this position.
      synthDesc :: !ByteString
    }
    -- | A command-line option.
  | CmdLine
  deriving (Ord, Eq)

filepath :: PositionInfo -> ByteString
filepath Span { spanFile = fname } = fname
filepath Point { pointFile = fname } = fname
filepath File { fileName = fname } = fname
filepath pos = error $! "Position " ++ show pos ++ " has no file name"

start :: PositionInfo -> (Word, Word)
start Point { pointLine = line, pointColumn = col } = (line, col)
start Span { spanStartLine = line, spanStartColumn = col } = (line, col)
start pos = error $! "Position " ++ show pos ++ " has no starting point"

end :: PositionInfo -> (Word, Word)
end Point { pointLine = line, pointColumn = col } = (line, col)
end Span { spanEndLine = line, spanEndColumn = col } = (line, col)
end pos = error $! "Position " ++ show pos ++ " has no ending point"

instance Semigroup PositionInfo where
  Point { pointFile = fname1, pointLine = line1, pointColumn = col1 } <>
    Point { pointFile = fname2, pointLine = line2, pointColumn = col2 }
    | fname1 == fname2 = Span { spanStartLine = line1, spanStartColumn = col1,
                                spanEndLine = line2, spanEndColumn = col2,
                                spanFile = fname1 }
    | otherwise = error "Cannot combine positions in different files"
  Span { spanFile = fname1, spanStartLine = line1, spanStartColumn = col1 } <>
    Point { pointFile = fname2, pointLine = line2, pointColumn = col2 }
    | fname1 == fname2 = Span { spanStartLine = line1, spanStartColumn = col1,
                                spanEndLine = line2, spanEndColumn = col2,
                                spanFile = fname1 }
    | otherwise = error "Cannot combine positions in different files"
  Point { pointFile = fname1, pointLine = line1, pointColumn = col1 } <>
    Span { spanFile = fname2, spanEndLine = line2, spanEndColumn = col2 }
    | fname1 == fname2 = Span { spanStartLine = line1, spanStartColumn = col1,
                                spanEndLine = line2, spanEndColumn = col2,
                                spanFile = fname1 }
    | otherwise = error "Cannot combine positions in different files"
  Span { spanFile = fname1, spanStartLine = line1, spanStartColumn = col1 } <>
    Span { spanFile = fname2, spanEndLine = line2, spanEndColumn = col2 }
    | fname1 == fname2 = Span { spanStartLine = line1, spanStartColumn = col1,
                                spanEndLine = line2, spanEndColumn = col2,
                                spanFile = fname1 }
    | otherwise = error "Cannot combine positions in different files"
  CmdLine <> CmdLine = CmdLine
  p1 <> p2 = error $! "Cannot combine positions " ++ show p1 ++ ", " ++ show p2

instance Format PositionInfo where
  format Span { spanStartLine = startline, spanStartColumn = startcol,
                spanEndLine = endline, spanEndColumn = endcol,
                spanFile = fname }
    | startline == endline =
      cat [ bytestring fname, char ':', format startline, char '.',
            format startcol, char '-', format endcol ]
    | otherwise =
      hcat [ bytestring fname, char ':', format startline, char '.',
             format startcol, char '-', format endline, char '.',
             format endcol ]
  format Point { pointLine = line, pointColumn = col, pointFile = fname } =
    hcat [ bytestring fname, char ':', format line, char '.', format col ]
  format File { fileName = fname } = bytestring fname
  format Synthetic { synthDesc = desc } = bytestring desc
  format CmdLine = string "command line"

instance Show PositionInfo where
  show = show . renderOneLine . format

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
  hashWithSalt s File { fileName = fname } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` fname
  hashWithSalt s Synthetic { synthDesc = desc } =
    s `hashWithSalt` (3 :: Word) `hashWithSalt` desc
  hashWithSalt s CmdLine = s `hashWithSalt` (4 :: Int)

spanPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] PositionInfo
spanPickler =
  let
    fwdfunc (((), startline, startcol, endline, endcol), fname) =
      Span { spanStartLine = startline, spanStartColumn = startcol,
             spanEndLine = endline, spanEndColumn = endcol,
             spanFile = fname }

    revfunc Span { spanStartLine = startline, spanStartColumn = startcol,
                   spanEndLine = endline, spanEndColumn = endcol,
                   spanFile = fname} =
      (((), startline, startcol, endline, endcol), fname)
    revfunc pinfo = error $! "Can't convert " ++ show pinfo
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xp5Tuple (xpAttrFixed (gxFromString "kind")
                                          (gxFromString "Span"))
                             (xpAttr (gxFromString "start-line") xpPrim)
                             (xpAttr (gxFromString "start-column") xpPrim)
                             (xpAttr (gxFromString "end-line") xpPrim)
                             (xpAttr (gxFromString "end-column") xpPrim))
                   (xpElemNodes (gxFromString "file") (xpContent xpPrim)))

pointPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] PositionInfo
pointPickler =
  let
    fwdfunc (((), line, col), fname) =
      Point { pointFile = fname, pointLine = line, pointColumn = col }

    revfunc Point { pointFile = fname, pointLine = line, pointColumn = col } =
      (((), line, col), fname)
    revfunc pinfo = error $! "Can't convert " ++ show pinfo
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpTriple (xpAttrFixed (gxFromString "kind")
                                          (gxFromString "Point"))
                             (xpAttr (gxFromString "column") xpPrim)
                             (xpAttr (gxFromString "line") xpPrim))
                   (xpElemNodes (gxFromString "file") (xpContent xpPrim)))

eofPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] PositionInfo
eofPickler =
  let
    revfunc File { fileName = fname } = ((), fname)
    revfunc pinfo = error $! "Can't convert " ++ show pinfo
  in
    xpWrap (File . snd, revfunc)
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

cmdLinePickler :: (GenericXMLString tag, Show tag,
                   GenericXMLString text, Show text) =>
                  PU [NodeG [] tag text] PositionInfo
cmdLinePickler =
  let
    revfunc CmdLine = ()
    revfunc pinfo = error $! "Can't convert " ++ show pinfo
  in
    xpWrap (const CmdLine, revfunc)
           (xpElemAttrs (gxFromString "PositionInfo")
                        (xpAttrFixed (gxFromString "kind")
                                     (gxFromString "CmdLine")))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] PositionInfo where
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
