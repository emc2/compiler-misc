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

-- | Defines a class of monads with access to information about
-- 'Position's.
module Control.Monad.Positions.Class(
       MonadPositions(..)
       ) where

import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString
import Data.Position
import Data.Word

-- | Class of monads with access to information about 'Position's.
class Monad m => MonadPositions m where
  -- | Indicate whether or not the 'Position' is synthetic
  -- (compiler-generated).
  positionIsSynthetic :: Position
                      -- ^ The 'Position'
                      -> m Bool
                      -- ^ @True@ if the position is synthetic.
  -- | Indicate the origin of the 'Position'.  For non-synthetic
  -- positions, this will be the name of the source file.  For
  -- synthetic positions, this will be a description of where in the
  -- compiler they were created.
  positionOrigin :: Position
                 -- ^ The 'Position'
                 -> m ByteString
                 -- ^ A UTF-8 string describing the origin of the position.
  -- | Indicate the line number and column number of the position.
  positionLineColumn :: Position
                     -- ^ The 'Position'
                     -> m (Word, Word)
                     -- ^ The line number and column number of the
                     -- position, respectively.

instance MonadPositions m => MonadPositions (ContT r m) where
  positionIsSynthetic = lift . positionIsSynthetic
  positionOrigin = lift . positionOrigin
  positionLineColumn = lift . positionLineColumn

instance (MonadPositions m, Error e) => MonadPositions (ErrorT e m) where
  positionIsSynthetic = lift . positionIsSynthetic
  positionOrigin = lift . positionOrigin
  positionLineColumn = lift . positionLineColumn

instance MonadPositions m => MonadPositions (ListT m) where
  positionIsSynthetic = lift . positionIsSynthetic
  positionOrigin = lift . positionOrigin
  positionLineColumn = lift . positionLineColumn

instance MonadPositions m => MonadPositions (ReaderT r m) where
  positionIsSynthetic = lift . positionIsSynthetic
  positionOrigin = lift . positionOrigin
  positionLineColumn = lift . positionLineColumn

instance MonadPositions m => MonadPositions (StateT s m) where
  positionIsSynthetic = lift . positionIsSynthetic
  positionOrigin = lift . positionOrigin
  positionLineColumn = lift . positionLineColumn

instance (MonadPositions m, Monoid w) => MonadPositions (WriterT w m) where
  positionIsSynthetic = lift . positionIsSynthetic
  positionOrigin = lift . positionOrigin
  positionLineColumn = lift . positionLineColumn
