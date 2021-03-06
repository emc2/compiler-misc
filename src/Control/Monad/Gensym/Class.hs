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
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Defines a class representing a monad which memoizes strings as
-- tokens.  Useful in a compiler context for representing symbols.
module Control.Monad.Gensym.Class(
       MonadGensym(..)
       ) where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Journal
import Control.Monad.Writer
import Data.ByteString
import Data.Symbol

-- | A class of monads that allow @ByteString@s to be memoized as @Symbol@s.
class Monad m => MonadGensym m where
  -- | Get the @Symbol@ for representing the given @ByteString@.  This
  -- function returns the same @Symbol@ each time it is called with a
  -- given name.
  symbol :: ByteString -> m Symbol
  -- | Create a unique symbol.  Note that even if this symbol ends up
  -- with the same name as another symbol, they will be distinct, and
  -- calls to @symbol@ will return a distinct @Symbol@.
  unique :: (Word -> ByteString) -> m Symbol

instance MonadGensym m => MonadGensym (ContT r m) where
  symbol = lift . symbol
  unique = lift . unique

instance (MonadGensym m) => MonadGensym (ExceptT e m) where
  symbol = lift . symbol
  unique = lift . unique

instance (MonadGensym m) => MonadGensym (JournalT e m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadGensym m => MonadGensym (ListT m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadGensym m => MonadGensym (ReaderT r m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadGensym m => MonadGensym (StateT s m) where
  symbol = lift . symbol
  unique = lift . unique

instance (MonadGensym m, Monoid w) => MonadGensym (WriterT w m) where
  symbol = lift . symbol
  unique = lift . unique
