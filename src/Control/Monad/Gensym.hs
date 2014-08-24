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
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Control.Monad.Gensym(
       MonadGensym(..),
       GensymT,
       Gensym,
       runGensymT,
       runGensym,
       startGensymT,
       startGensym,
       runSymbolsT,
       runSymbols,
       ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Gensym.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Symbols.Class
import Control.Monad.Symbols(Symbols, SymbolsT)
import Data.ByteString.Char8 hiding (map, empty)
import Data.HashTable.IO(BasicHashTable)
import Data.Symbol

import qualified Control.Monad.Symbols as Symbols
import qualified Data.HashTable.IO as HashTable

-- | Lookup tables for converting strings to symbols and back.
data Tables =
  Tables {
    revTable :: !(BasicHashTable Symbol ByteString),
    fwdTable :: !(BasicHashTable ByteString Symbol)
  }

data Bounds = Bounds { loBound :: !Symbol, hiBound :: !Symbol }

newtype GensymT m a =
  GensymT { unpackGensymT :: StateT Bounds (ReaderT Tables m) a }

type Gensym = GensymT IO

initBounds :: (Symbol, Symbol)
initBounds = (firstSym, succ firstSym)

-- | Execute the computation represented by a Symbols monad.
runGensym :: Gensym a
           -- ^ The Symbols monad to execute.
           -> (Symbol, Symbol)
           -- ^ The low and high range of the symbols.
           -> [(Symbol, ByteString)]
           -- ^ The mapping of symbols.  The mapping to the lowest
           -- index is taken as the null symbol.
           -> IO a
runGensym s = runGensymT s

-- | Execute the computation wrapped in a SymbolsT monad transformer.
runGensymT :: MonadIO m =>
              GensymT m a
           -- ^ The SymbolsT monad to execute.
           -> (Symbol, Symbol)
           -- ^ The low and high range of the symbols.  The lowest
           -- index is used as the index of the null symbol.
           -> [(Symbol, ByteString)]
           -- ^ The mapping of symbols to indexes.  The mapping to the
           -- lowest index is taken as the null symbol.
           -> m a
runGensymT s (lo, hi) vals =
  do
    fwdtab <- liftIO (HashTable.fromList (map (\(a, b) -> (b, a)) vals))
    revtab <- liftIO (HashTable.fromList vals)
    (out, _) <- runReaderT (runStateT (unpackGensymT s)
                                      Bounds { loBound = lo, hiBound = hi })
                           Tables { revTable = revtab, fwdTable = fwdtab }
    return out

-- | Execute a Gensym monad with a starting state with only the null
-- symbol defined
startGensym :: Gensym a
            -- ^ The Symbols monad to execute.
            -> IO a
startGensym s = startGensymT s

-- | Execute a GensymT monad transformer with a starting state with
-- only the null symbol defined
startGensymT :: MonadIO m =>
                GensymT m a
             -- ^ The SymbolsT monad to execute.
             -> m a
startGensymT s =
  runGensymT s initBounds [(firstSym, pack "<null>")]

-- | Execute a SymbolsT monad transformer using the current symbol
-- state.
runSymbolsT :: MonadIO m =>
               SymbolsT m a
            -- ^ The Symbols monad to execute.
            -> GensymT m a
runSymbolsT = GensymT . runSymbolsT'

runSymbolsT' :: MonadIO m => SymbolsT m a -> StateT Bounds (ReaderT Tables m) a
runSymbolsT' s =
  do
    Bounds { loBound = lo, hiBound = hi } <- get
    Tables { revTable = tab } <- ask
    vals <- liftIO (HashTable.toList tab)
    lift (lift (Symbols.runSymbolsT s (lo, hi) vals))

-- | Execute a Symbols monad using the current symbol state.
runSymbols :: Symbols a
           -- ^ The Symbols monad to execute.
           -> Gensym a
runSymbols s = runSymbolsT s

-- Get the next symbol
nextSym :: Monad m => (StateT Bounds (ReaderT Tables m)) Symbol
nextSym =
  do
    bounds @ Bounds { hiBound = hi } <- get
    put bounds { hiBound = succ hi }
    return hi

symbol' :: MonadIO m => ByteString -> (StateT Bounds (ReaderT Tables m)) Symbol
symbol' str =
  do
    Tables { fwdTable = tab } <- ask
    sym <- liftIO (HashTable.lookup tab str)
    case sym of
      Just out -> return out
      Nothing ->
        do
          newsym <- nextSym
          liftIO (HashTable.insert tab str newsym)
          return newsym

nullSym' :: Monad m => StateT Bounds (ReaderT Tables m) Symbol
nullSym' =
  do
    Bounds { loBound = out } <- get
    return out

allNames' :: MonadIO m => StateT Bounds (ReaderT Tables m) [ByteString]
allNames' =
  do
    Tables { fwdTable = tab } <- ask
    entries <- liftIO (HashTable.toList tab)
    return (map fst entries)

name' :: MonadIO m => Symbol -> StateT Bounds (ReaderT Tables m) ByteString
name' sym =
  do
    Tables { revTable = tab } <- ask
    symname <- liftIO (HashTable.lookup tab sym)
    case symname of
      Just out -> return out
      Nothing -> error "Undefined symbol"

allSyms' :: MonadIO m => StateT Bounds (ReaderT Tables m) [Symbol]
allSyms' =
  do
    Tables { revTable = tab } <- ask
    entries <- liftIO (HashTable.toList tab)
    return (map fst entries)

instance Monad m => Monad (GensymT m) where
  return = GensymT . return
  s >>= f = GensymT $ unpackGensymT s >>= unpackGensymT . f

instance Monad m => Applicative (GensymT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) => Alternative (GensymT m) where
  empty = lift empty
  s1 <|> s2 = GensymT ((unpackGensymT s1) <|> (unpackGensymT s2))

instance Functor (GensymT m) where
  fmap f = fmap f

instance MonadIO m => MonadSymbols (GensymT m) where
  nullSym = GensymT nullSym'
  allNames = GensymT allNames'
  allSyms = GensymT allSyms'
  name = GensymT . name'

instance MonadIO m => MonadGensym (GensymT m) where
  symbol = GensymT . symbol'

instance MonadIO m => MonadIO (GensymT m) where
  liftIO = GensymT . liftIO

instance MonadTrans GensymT where
  lift = GensymT . lift . lift

instance MonadCont m => MonadCont (GensymT m) where
  callCC f = GensymT (callCC (\c -> unpackGensymT (f (GensymT . c))))

instance (Error e, MonadError e m) => MonadError e (GensymT m) where
  throwError = lift . throwError
  m `catchError` h =
    GensymT (unpackGensymT m `catchError` (unpackGensymT . h))

instance MonadState s m => MonadState s (GensymT m) where
  get = lift get
  put = lift . put

instance MonadPlus m => MonadPlus (GensymT m) where
  mzero = lift mzero
  mplus s1 s2 = GensymT (mplus (unpackGensymT s1) (unpackGensymT s2))

instance MonadFix m => MonadFix (GensymT m) where
  mfix f = GensymT (mfix (unpackGensymT . f))
