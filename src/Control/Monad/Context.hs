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
{-# OPTIONS_GHC -Wall -Werror #-}

-- | Defines monads that store read-only context information.  This is
-- intended to be used to implement a compiler backend, after all
-- lexing has been done.
module Control.Monad.Context(
       ContextT,
       Context,
       runContextT) where

import Control.Monad.Comments
import Control.Monad.Positions
import Control.Monad.SourceFiles
import Control.Monad.Symbols
import Control.Monad.Trans
import Data.Array
import Data.ByteString
import Data.HashTable.IO(BasicHashTable)
import Data.Position.Filename
import Data.Position.Point
import Data.Symbol
import Data.Word

-- | A monad transformer representing all context information that
-- would be gathered by the frontend.
type ContextT m a = (CommentsT (PositionsT (SymbolsT (SourceFilesT m)))) a

type Context a = ContextT IO a

-- | Execute a computation wrapped in a ContextT monad transformer,
-- given all the starting information.
runContextT :: MonadIO m =>
               ContextT m a
            -- ^ The context monad transformer to execute.
            -> Array Filename (Array Word ByteString)
            -- ^ An array mapping 'FilePath's to source file contents.
            -> BasicHashTable Point [ByteString]
            -- ^ A hash table from 'Point's to comment data.
            -> (Point, Point)
            -- ^ The low and high range of the symbols.
            -> [(Point, PointInfo)]
            -- ^ The mapping of symbols.  The mapping to the lowest
            -- index is taken as the null symbol.
            -> (Filename, Filename)
            -- ^ The low and high range of the symbols.
            -> [(Filename, FileInfo)]
            -- ^ The mapping of symbols.  The mapping to the lowest
            -- index is taken as the null symbol.
            -> (Symbol, Symbol)
            -- ^ The low and high range of the symbols.  The lowest
            -- index is used as the index of the null symbol.
            -> [(Symbol, ByteString)]
            -- ^ The mapping of symbols to indexes.  The mapping to the
            -- lowest index is taken as the null symbol.
            -> m a
runContextT c sourcefiles comments pointrange points
            filerange filenames symrange symbols =
  let
    p = runCommentsT c comments
    s = runPositionsT p pointrange points filerange filenames
    f = runSymbolsT s symrange symbols
  in
    runSourceFilesT f sourcefiles
