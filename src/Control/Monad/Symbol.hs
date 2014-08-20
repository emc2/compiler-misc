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
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             FlexibleInstances, UndecidableInstances #-}

module Control.Monad.Symbol(
       MonadSymbol(..),
       SymbolT,
       Symbol,
       Sym,
       runSymbolT,
       runSymbol
       ) where

import Control.Monad.Reader
import Control.Monad.Symbol.Class
import Control.Monad.State
import Control.Monad.Trans
import Data.Array
import Data.ByteString
import Data.Hashable
import Data.Word

newtype SymbolT m a =
  SymbolT { unpackSymbolT :: (ReaderT (Array Word ByteString) m) a }

type Symbol a = SymbolT IO a

-- | Type used for symbols.
newtype Sym = Sym { symID :: Word }
  deriving (Eq, Ord, Ix)

-- | Execute the computation represented by a gensym monad.
runSymbol :: Symbol a
          -- ^ The SymbolT monad to execute.
          -> (Word, Word)
          -- ^ The low and high range of the symbols.
          -> [(Word, ByteString)]
          -- ^ The mapping of symbols.  The mapping to the lowest
          -- index is taken as the null symbol.
          -> IO a
runSymbol s = runSymbolT s

-- | Execute the computation wrapped in a gensym monad transformer.
runSymbolT :: MonadIO m =>
              SymbolT m a
           -- ^ The SymbolT monad to execute.
           -> (Word, Word)
           -- ^ The low and high range of the symbols.  The lowest
           -- index is used as the index of the null symbol.
           -> [(Word, ByteString)]
           -- ^ The mapping of symbols to indexes.  The mapping to the
           -- lowest index is taken as the null symbol.
           -> m a
runSymbolT s = runReaderT s . array

nullSym' :: Monad m => (ReaderT (Array Word ByteString) m) Sym
nullSym' = ask >>= return . Sym . fst . bounds

allNames' :: Monad m => (ReaderT (Array Word ByteString) m) [ByteString]
allNames' = ask >>= return . elems

name' :: Monad m => Sym -> (ReaderT (Array Word ByteString) m) ByteString
name' Sym { symID = num } = ask >>= return . (! num)

instance Enum Sym where
  succ = Sym . succ . symID
  pred = Sym . pred . symID
  toEnum = Sym . toEnum
  fromEnum = fromEnum . symID
  enumFromThen Sym { symID = n } = enumFromThen n . symID
  enumFromTo Sym { symID = n } = enumFromTo n . symID
  enumFromThenTo Sym { symID = n } Sym { symID = m } = enumFromThenTo n m . symID

instance Hashable Sym where
  hashWithSalt s = hashWithSalt s . symID

instance Monad m => Monad (SymbolT m) where
  return = SymbolT . return
  s >>= f = SymbolT $ s >>= unpackSymbolT . f

instance Monad m => MonadSymbol Sym (SymbolT m) where
  nullSym = SymbolT nullSym'
  allNames = SymbolT allNames'
  name = SymbolT . name'

instance MonadIO m => MonadIO (SymbolT m) where
  liftIO = SymbolT . liftIO

instance MonadTrans SymbolT where
  lift = SymbolT . lift

instance MonadSymbol Sym m => MonadSymbol Sym (StateT s m) where
  allNames = lift allNames
  name = lift . name

instance MonadState s m => MonadState s (SymbolT m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (SymbolT m) where
  ask = lift ask
  local f = SymbolT . mapReaderT (local f) . unpackSymbolT
