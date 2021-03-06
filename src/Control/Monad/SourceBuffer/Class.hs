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

-- | Defines a class of monads that buffer source.  Useful for
module Control.Monad.SourceBuffer.Class(
       MonadSourceBuffer(..)
       ) where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Journal
import Control.Monad.Writer
import Data.Position.Filename

import qualified Data.ByteString.Lazy as Lazy

-- | Class of monads that store source file contents during lexing and
-- split it into lines, which are then subsequently stored and
-- accessed through a 'MonadSourceFiles' interface.
class Monad m => MonadSourceBuffer m where
  -- | Report a linebreak at the given offset in the file.
  linebreak :: Int
            -- ^ The offset of the linebreak.
            -> m()
  -- | Start a new file.
  startFile :: Filename
            -- ^ The file name.
            -> Lazy.ByteString
            -- ^ The file contents.
            -> m ()
  -- | Complete the file, adding the last line, and storing the split
  -- lines.
  finishFile :: m ()

instance MonadSourceBuffer m => MonadSourceBuffer (ContT r m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance (MonadSourceBuffer m) => MonadSourceBuffer (ExceptT e m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance (MonadSourceBuffer m) => MonadSourceBuffer (JournalT e m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadSourceBuffer m => MonadSourceBuffer (ListT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadSourceBuffer m => MonadSourceBuffer (ReaderT r m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile


instance MonadSourceBuffer m => MonadSourceBuffer (StateT s m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile


instance (MonadSourceBuffer m, Monoid w) =>
         MonadSourceBuffer (WriterT w m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile
