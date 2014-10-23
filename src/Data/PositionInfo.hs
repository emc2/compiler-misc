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
    -- | A specific line and column in a source file.
    SourcePosition {
      -- | The line number, starting at 1.
      srcLine :: !Word,
      -- | The column number, staring at 1.
      srcColumn :: !Word,
      -- | The name of the source file.
      srcFile :: !ByteString
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
  show SourcePosition { srcLine = line, srcColumn = col, srcFile = fname } =
    show fname ++ ":" ++ show line ++ "." ++ show col
  show EndOfFile { eofFile = fname } = show fname ++ ": end of input"
  show Synthetic { synthDesc = desc } = show desc

instance Hashable PositionInfo where
  hashWithSalt s SourcePosition { srcLine = line, srcColumn = col,
                                  srcFile = fname } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt`
    line `hashWithSalt` col `hashWithSalt` fname
  hashWithSalt s EndOfFile { eofFile = fname } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` fname
  hashWithSalt s Synthetic { synthDesc = desc } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` desc

srcPosPickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] PositionInfo
srcPosPickler =
  let
    fwdfunc ((), (line, col, fname)) =
      SourcePosition { srcFile = fname, srcLine = line, srcColumn = col }

    revfunc SourcePosition { srcFile = fname, srcLine = line,
                             srcColumn = col } = ((), (line, col, fname))
    revfunc pinfo = error $! "Can't convert " ++ show pinfo
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpAttrFixed (gxFromString "kind")
                                (gxFromString "SourcePosition"))
                   (xpContent xpPrim))

eofPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text) =>
              PU [NodeG [] tag text] PositionInfo
eofPickler =
  let
    fwdfunc ((), fname) = EndOfFile { eofFile = fname }

    revfunc EndOfFile { eofFile = fname } = ((), fname)
    revfunc pinfo = error $! "Can't convert " ++ show pinfo
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpAttrFixed (gxFromString "kind")
                                (gxFromString "EndOfFile"))
                   (xpContent xpPrim))

syntheticPickler :: (GenericXMLString tag, Show tag,
                     GenericXMLString text, Show text) =>
                    PU [NodeG [] tag text] PositionInfo
syntheticPickler =
  let
    fwdfunc ((), desc) = Synthetic { synthDesc = desc }

    revfunc Synthetic { synthDesc = desc } = ((), desc)
    revfunc pinfo = error $! "Can't convert " ++ show pinfo
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "PositionInfo")
                   (xpAttrFixed (gxFromString "kind")
                                (gxFromString "Synthetic"))
                   (xpContent xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] PositionInfo where
  xpickle =
    let
      picker SourcePosition {} = 0
      picker EndOfFile {} = 1
      picker Synthetic {} = 2
    in
      xpAlt picker [srcPosPickler, eofPickler, syntheticPickler]
