-- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
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

-- | Defines a class of monads representing access to source code.
-- This is useful for implementing lexers, as well as implementing a
-- diagnostic message printer.
module Control.Monad.SourceFiles.Class(
       MonadSourceFiles(..)
       ) where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Journal
import Control.Monad.Writer
import Data.Array
import Data.ByteString hiding (map)
import Data.Position.Filename

-- | Class of monads that have access to source code.
class Monad m => MonadSourceFiles m where
  -- | Get all lines from the source file.
  sourceFile :: Filename
             -- ^ The path to the source file.
             -> m (Array Word ByteString)
             -- ^ An array of all lines in the source file.

  sourceFileSpan :: Filename
                 -- ^ The path to the source file.
                 -> Word
                 -- ^ The starting line
                 -> Word
                 -- ^ The ending line
                 -> m [ByteString]
  sourceFileSpan fpath start end =
    do
      fdata <- sourceFile fpath
      return $! map (fdata !) [start..end]

instance MonadSourceFiles m => MonadSourceFiles (ContT r m) where
  sourceFile = lift . sourceFile

instance (MonadSourceFiles m) => MonadSourceFiles (ExceptT e m) where
  sourceFile = lift . sourceFile

instance (MonadSourceFiles m) => MonadSourceFiles (JournalT e m) where
  sourceFile = lift . sourceFile

instance MonadSourceFiles m => MonadSourceFiles (ListT m) where
  sourceFile = lift . sourceFile

instance MonadSourceFiles m => MonadSourceFiles (ReaderT r m) where
  sourceFile = lift . sourceFile

instance MonadSourceFiles m => MonadSourceFiles (StateT s m) where
  sourceFile = lift . sourceFile

instance (MonadSourceFiles m, Monoid w) => MonadSourceFiles (WriterT w m) where
  sourceFile = lift . sourceFile
