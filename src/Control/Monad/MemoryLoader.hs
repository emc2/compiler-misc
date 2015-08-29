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
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             FlexibleInstances, UndecidableInstances #-}

-- | Provides a 'MonadLoader' instance that simply reports the
-- contents of an in-memory table.  This is primarily intended for
-- testing, to allow a compiler using the 'MonadLoader' framework to
-- be tested without ever using the filesystem.
module Control.Monad.MemoryLoader(
       MonadLoader(..),
       MemoryLoaderT,
       MemoryLoader,
       runMemoryLoaderT,
       runMemoryLoader,
       mapMemoryLoaderT
       ) where

import Control.Applicative
import Control.Monad.Artifacts.Class
import Control.Monad.CommentBuffer.Class
import Control.Monad.Comments.Class
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Genpos.Class
import Control.Monad.Gensym.Class
import Control.Monad.Keywords.Class
import Control.Monad.Loader.Class
import Control.Monad.Messages.Class
import Control.Monad.Positions.Class
import Control.Monad.Reader
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceBuffer.Class
import Control.Monad.State
import Control.Monad.Symbols.Class
import Control.Monad.Writer
import Data.HashTable.IO(BasicHashTable)
import Data.Position.Filename
import System.IO.Error

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict
import qualified Data.HashTable.IO as HashTable

type Table info = BasicHashTable Strict.ByteString info

newtype MemoryLoaderT info m a =
  MemoryLoaderT { unpackMemoryLoaderT :: ReaderT (Table info) m a }

type MemoryLoader info = MemoryLoaderT info IO

-- | Execute the computation represented by a MemoryLoader monad.
runMemoryLoader :: MemoryLoader info a
                -- ^ The MemoryLoaderT monad to execute.
                -> Table info
                -- ^ A hash table containing the contents of all files.
                -> IO a
runMemoryLoader = runMemoryLoaderT

-- | Execute the computation wrapped in a MemoryLoaderT monad transformer.
runMemoryLoaderT :: MonadIO m =>
                    MemoryLoaderT info m a
                 -- ^ The MemoryLoaderT monad to execute.
                 -> Table info
                 -- ^ A hash table containing the contents of all files.
                 -> m a
runMemoryLoaderT s = runReaderT (unpackMemoryLoaderT s)

mapMemoryLoaderT :: (Monad m, Monad n) =>
                    (m a -> n b) -> MemoryLoaderT info m a ->
                    MemoryLoaderT info n b
mapMemoryLoaderT f = MemoryLoaderT . mapReaderT f . unpackMemoryLoaderT

load' :: (MonadIO m, MonadGenpos m) => Strict.ByteString ->
         ReaderT (Table info) m (Either IOError (Filename, info))
load' path =
  let
    fpath = Strict.toString path
  in do
    table <- ask
    entry <- liftIO (HashTable.lookup table path)
    case entry of
      Just out ->
        do
          fname <- filename FileInfo { fileInfoName = path,
                                       fileInfoDir = Strict.empty }
          return $! Right (fname,  out)
      Nothing -> return $! Left $! mkIOError doesNotExistErrorType
                                             "No such entry"
                                             Nothing (Just fpath)

instance Monad m => Monad (MemoryLoaderT info m) where
  return = MemoryLoaderT . return
  s >>= f = MemoryLoaderT $ unpackMemoryLoaderT s >>= unpackMemoryLoaderT . f

instance Monad m => Applicative (MemoryLoaderT info m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) =>
         Alternative (MemoryLoaderT info m) where
  empty = lift empty
  s1 <|> s2 =
    MemoryLoaderT (unpackMemoryLoaderT s1 <|> unpackMemoryLoaderT s2)

instance Functor (MemoryLoaderT info m) where
  fmap = fmap

instance MonadIO m => MonadIO (MemoryLoaderT info m) where
  liftIO = MemoryLoaderT . liftIO

instance MonadTrans (MemoryLoaderT info) where
  lift = MemoryLoaderT . lift

instance (MonadIO m, MonadGenpos m) =>
         MonadLoader Strict.ByteString info (MemoryLoaderT info m) where
  load = MemoryLoaderT . load'

instance MonadArtifacts path m =>
         MonadArtifacts path (MemoryLoaderT info m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCommentBuffer m =>
         MonadCommentBuffer (MemoryLoaderT info m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (MemoryLoaderT info m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (MemoryLoaderT info m) where
  callCC f =
    MemoryLoaderT (callCC (\c -> unpackMemoryLoaderT (f (MemoryLoaderT . c))))

instance (MonadError e m) => MonadError e (MemoryLoaderT info m) where
  throwError = lift . throwError
  m `catchError` h =
    MemoryLoaderT (unpackMemoryLoaderT m `catchError` (unpackMemoryLoaderT . h))

instance MonadGenpos m => MonadGenpos (MemoryLoaderT info m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (MemoryLoaderT info m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadKeywords p t m => MonadKeywords p t (MemoryLoaderT info m) where
  mkKeyword p = lift . mkKeyword p

instance MonadMessages msg m => MonadMessages msg (MemoryLoaderT info m) where
  message = lift . message

instance MonadPositions m => MonadPositions (MemoryLoaderT info m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadSourceFiles m =>
         MonadSourceFiles (MemoryLoaderT info m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m =>
         MonadSourceBuffer (MemoryLoaderT info m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (MemoryLoaderT info m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (MemoryLoaderT info m) where
  ask = lift ask
  local f = mapMemoryLoaderT (local f)

instance MonadSymbols m => MonadSymbols (MemoryLoaderT info m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadWriter w m => MonadWriter w (MemoryLoaderT info m) where
  tell = lift . tell
  listen = mapMemoryLoaderT listen
  pass = mapMemoryLoaderT pass

instance MonadPlus m => MonadPlus (MemoryLoaderT info m) where
  mzero = lift mzero
  mplus s1 s2 = MemoryLoaderT (mplus (unpackMemoryLoaderT s1)
                                     (unpackMemoryLoaderT s2))

instance MonadFix m => MonadFix (MemoryLoaderT info m) where
  mfix f = MemoryLoaderT (mfix (unpackMemoryLoaderT . f))
