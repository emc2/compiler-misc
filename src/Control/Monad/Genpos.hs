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

module Control.Monad.Genpos(
       MonadGenpos(..),
       GenposT,
       Genpos,
       runGenposT,
       runGenpos,
       startGenposT,
       startGenpos,
       ) where

import Control.Applicative
import Control.Monad.CommentBuffer.Class
import Control.Monad.Comments.Class
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Genpos.Class
import Control.Monad.Gensym.Class
import Control.Monad.Keywords.Class
import Control.Monad.Positions.Class
import Control.Monad.Reader
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceLoader.Class
import Control.Monad.State
import Control.Monad.Symbols.Class
import Data.ByteString.Char8 hiding (map, empty)
import Data.HashTable.IO(BasicHashTable)
import Data.Position
import Data.PositionInfo
import Data.Word

import qualified Data.HashTable.IO as HashTable

data Bounds = Bounds { loBound :: !Position, hiBound :: !Position }

type Table = BasicHashTable Position PositionInfo

newtype GenposT m a =
  GenposT { unpackGenposT :: StateT Bounds (ReaderT Table m) a }

type Genpos = GenposT IO

initBounds :: (Position, Position)
initBounds = (firstPosition, firstPosition)

-- | Execute the computation represented by a Genpos monad.
runGenpos :: Genpos a
           -- ^ The Symbols monad to execute.
           -> (Position, Position)
           -- ^ The low and high range of the positions.
           -> [(Position, PositionInfo)]
           -- ^ The mapping of symbols.  The mapping to the lowest
           -- index is taken as the null symbol.
           -> IO a
runGenpos = runGenposT

-- | Execute the computation wrapped in a GenposT monad transformer.
runGenposT :: MonadIO m =>
              GenposT m a
           -- ^ The SymbolsT monad to execute.
           -> (Position, Position)
           -- ^ The low and high range of the positions.  The lowest
           -- index is used as the index of the null symbol.
           -> [(Position, PositionInfo)]
           -- ^ The mapping of symbols to indexes.  The mapping to the
           -- lowest index is taken as the null symbol.
           -> m a
runGenposT s (lo, hi) vals =
  do
    tab <- liftIO (HashTable.fromList vals)
    (out, _) <- runReaderT (runStateT (unpackGenposT s)
                                      Bounds { loBound = lo, hiBound = hi }) tab
    return out

-- | Execute a Genpos monad with a starting state with only the null
-- symbol defined
startGenpos :: Genpos a
            -- ^ The Symbols monad to execute.
            -> IO a
startGenpos = startGenposT

-- | Execute a GenposT monad transformer with a starting state with
-- only the null symbol defined
startGenposT :: MonadIO m =>
                GenposT m a
             -- ^ The SymbolsT monad to execute.
             -> m a
startGenposT s =
  runGenposT s initBounds []

position' :: MonadIO m =>
           ByteString
        -> Word
        -> Word
        -> (StateT Bounds (ReaderT Table m)) Position
position' fname line col =
  do
    tab <- ask
    bounds @ Bounds { hiBound = hi } <- get
    put bounds { hiBound = succ hi }
    liftIO (HashTable.insert tab hi SourcePosition { srcFile = fname,
                                                     srcLine = line,
                                                     srcColumn = col })
    return hi

instance Monad m => Monad (GenposT m) where
  return = GenposT . return
  s >>= f = GenposT $ unpackGenposT s >>= unpackGenposT . f

instance Monad m => Applicative (GenposT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) => Alternative (GenposT m) where
  empty = lift empty
  s1 <|> s2 = GenposT (unpackGenposT s1 <|> unpackGenposT s2)

instance Functor (GenposT m) where
  fmap = fmap

instance MonadIO m => MonadGenpos (GenposT m) where
  position fname line = GenposT . position' fname line

instance MonadCommentBuffer m => MonadCommentBuffer (GenposT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadIO m => MonadIO (GenposT m) where
  liftIO = GenposT . liftIO

instance MonadTrans GenposT where
  lift = GenposT . lift . lift

instance MonadComments m => MonadComments (GenposT m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (GenposT m) where
  callCC f = GenposT (callCC (\c -> unpackGenposT (f (GenposT . c))))

instance (Error e, MonadError e m) => MonadError e (GenposT m) where
  throwError = lift . throwError
  m `catchError` h =
    GenposT (unpackGenposT m `catchError` (unpackGenposT . h))

instance MonadGensym m => MonadGensym (GenposT m) where
  symbol = lift . symbol

instance MonadKeywords t m => MonadKeywords t (GenposT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadPositions m => MonadPositions (GenposT m) where
  positionInfo = lift . positionInfo

instance MonadSourceFiles m => MonadSourceFiles (GenposT m) where
  sourceLines = lift . sourceLines

instance MonadSourceLoader m => MonadSourceLoader (GenposT m) where
  loadSourceFile = lift . loadSourceFile

instance MonadState s m => MonadState s (GenposT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (GenposT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadPlus m => MonadPlus (GenposT m) where
  mzero = lift mzero
  mplus s1 s2 = GenposT (mplus (unpackGenposT s1) (unpackGenposT s2))

instance MonadFix m => MonadFix (GenposT m) where
  mfix f = GenposT (mfix (unpackGenposT . f))
