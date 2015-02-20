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

-- | Provides an abstract class for monads that locate and provide
-- source given a path.  This is an abstract interface for the
-- mechanism that locates source files, given some set of directories,
-- and then loads them.
module Control.Monad.Loader.Class(
       MonadLoader(..)
       ) where

import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- | A class of monads that locate and provide source given a path.
class Monad m => MonadLoader path info m where
  -- | Load the source specified by the given path.
  load :: path -> m (Either IOError info)

instance MonadLoader path info m => MonadLoader path info (ContT r m) where
  load = lift . load

instance (MonadLoader path info m, Error e) =>
         MonadLoader path info (ErrorT e m) where
  load = lift . load

instance MonadLoader path info m => MonadLoader path info (ListT m) where
  load = lift . load

instance MonadLoader path info m => MonadLoader path info (ReaderT r m) where
  load = lift . load

instance MonadLoader path info m => MonadLoader path info (StateT s m) where
  load = lift . load

instance (MonadLoader path info m, Monoid w) =>
         MonadLoader path info (WriterT w m) where
  load = lift . load
