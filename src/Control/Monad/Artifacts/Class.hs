-- Copyright (c) 2015 Eric McCorkle.  All rights reserved.
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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Provides an abstract class for monads that create artifacts
-- (results of compilation).  The most common use of this is to
-- produce object files.
module Control.Monad.Artifacts.Class(
       MonadArtifacts(..)
       ) where

import Blaze.ByteString.Builder
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

-- | A class of monads that create artifacts.
class Monad m => MonadArtifacts path m where
  -- | Create an artifact with the given pathname from a Builder.
  artifact :: path
           -- ^ The path for the artifact.
           -> Builder
           -- ^ A 'Builder' for the content of the artifact.
           -> m (Maybe IOError)

  -- | Create an artifact with a strict bytestring as contents.
  artifactBytestring :: path
                     -- ^ The path for the artifact.
                     ->  Strict.ByteString
                     -- ^ The contents of the artifact.
                     -> m (Maybe IOError)
  artifactBytestring path = artifact path . fromByteString

  -- | Create an artifact with a strict bytestring as contents.
  artifactLazyBytestring :: path
                         -- ^ The path for the artifact.
                         ->  Lazy.ByteString
                         -- ^ The contents of the artifact.
                         -> m (Maybe IOError)
  artifactLazyBytestring path = artifact path . fromLazyByteString

instance MonadArtifacts path m => MonadArtifacts path (ContT r m) where
  artifact path = lift . artifact path

instance (MonadArtifacts path m, Error e) =>
         MonadArtifacts path (ErrorT e m) where
  artifact path = lift . artifact path

instance MonadArtifacts path m => MonadArtifacts path (ListT m) where
  artifact path = lift . artifact path

instance MonadArtifacts path m => MonadArtifacts path (ReaderT r m) where
  artifact path = lift . artifact path

instance MonadArtifacts path m => MonadArtifacts path (StateT s m) where
  artifact path = lift . artifact path

instance (MonadArtifacts path m, Monoid w) =>
         MonadArtifacts path (WriterT w m) where
  artifact path = lift . artifact path
