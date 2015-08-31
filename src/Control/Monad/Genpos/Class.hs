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

-- | Defines a class of monads with the ability to create new
-- 'Position's.
module Control.Monad.Genpos.Class(
       MonadGenpos(..)
       ) where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.List
import Control.Monad.Positions.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Journal
import Control.Monad.Writer
import Data.Position.Filename
import Data.Position.Point

-- | An extension to the 'MonadPositions' class that adds the ability
-- to create new 'Point's.
class MonadPositions m => MonadGenpos m where
  -- | Create a 'Point' from raw data.
  point :: PointInfo
        -- ^ The position info.
        -> m Point

  -- | Create a 'Filename' from raw data.
  filename :: FileInfo
           -- ^ The file name and base path.
           -> m Filename

instance MonadGenpos m => MonadGenpos (ContT r m) where
  point = lift . point
  filename = lift . filename

instance (MonadGenpos m) => MonadGenpos (ExceptT e m) where
  point = lift . point
  filename = lift . filename

instance (MonadGenpos m) => MonadGenpos (JournalT e m) where
  point = lift . point
  filename = lift . filename

instance MonadGenpos m => MonadGenpos (ListT m) where
  point = lift . point
  filename = lift . filename

instance MonadGenpos m => MonadGenpos (ReaderT r m) where
  point = lift . point
  filename = lift . filename

instance MonadGenpos m => MonadGenpos (StateT s m) where
  point = lift . point
  filename = lift . filename

instance (Monoid w, MonadGenpos m) => MonadGenpos (WriterT w m) where
  point = lift . point
  filename = lift . filename
