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
module Data.Position.Filename(
       Filename,
       FileInfo(..),
       firstFilename,
       ) where

import Data.Hashable
import Data.Ix
import Data.Word
import Text.Format
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

import qualified Data.ByteString as Strict

-- | This is a token used to represent a file name.  This can be used
-- to look up a 'FileInfo' inside a 'Positions' monad instance.
newtype Filename =
  Filename {
    fIdx :: Word
  }
  deriving (Eq, Ord, Ix)

-- | Information about a file.
data FileInfo =
  FileInfo {
    -- | The (relative) file name.
    fileInfoName :: !Strict.ByteString,
    -- | The base directory.
    fileInfoDir :: !Strict.ByteString
  }
  deriving (Ord, Eq)

firstFilename :: Filename
firstFilename = Filename { fIdx = 0 }

instance Enum Filename where
  succ = Filename . succ . fIdx
  pred = Filename . pred . fIdx
  toEnum = Filename . toEnum
  fromEnum = fromEnum . fIdx
  enumFromThen Filename { fIdx = n } = map Filename . enumFromThen n . fIdx
  enumFromTo Filename { fIdx = n } = map Filename . enumFromTo n . fIdx
  enumFromThenTo Filename { fIdx = n } Filename { fIdx = m } =
    map Filename . enumFromThenTo n m . fIdx

instance Hashable Filename where
  hashWithSalt s Filename { fIdx = n } = hashWithSalt s n

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] Filename where
  xpickle = xpWrap (Filename, fIdx) (xpElemNodes (gxFromString "Filename")
                                                (xpContent xpPrim))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler (Attributes tag text) Filename where
  xpickle = xpWrap (Filename, fIdx) (xpAttr (gxFromString "filename") xpPrim)

instance Format FileInfo where
  format FileInfo { fileInfoName = fname, fileInfoDir = dir } =
    cat [bytestring dir, bytestring fname]

instance Hashable FileInfo where
  hashWithSalt s FileInfo { fileInfoName = fname, fileInfoDir = dir } =
    s `hashWithSalt` fname `hashWithSalt` dir

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
