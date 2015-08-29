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

-- | Provides a 'MonadArtifacts' instance that simply reports the
-- contents of an in-memory table.  This is primarily intended for
-- testing, to allow a compiler using the 'MonadArtifacts' framework to
-- be tested without ever using the filesystem.
module Control.Monad.MemoryArtifacts(
       MonadArtifacts(..),
       MemoryArtifactsT,
       MemoryArtifacts,
       runMemoryArtifactsT,
       runMemoryArtifacts,
       mapMemoryArtifactsT
       ) where

import Blaze.ByteString.Builder
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
import Data.Hashable
import Data.HashTable.IO(BasicHashTable)

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashTable.IO as HashTable

type Table path = BasicHashTable path Strict.ByteString

newtype MemoryArtifactsT path m a =
  MemoryArtifactsT { unpackMemoryArtifactsT :: ReaderT (Table path) m a }

type MemoryArtifacts path = MemoryArtifactsT path IO

-- | Execute the computation represented by a MemoryArtifacts monad.
runMemoryArtifacts :: MemoryArtifacts path a
                   -- ^ The MemoryArtifactsT monad to execute.
                   -> IO (a, Table path)
runMemoryArtifacts = runMemoryArtifactsT

-- | Execute the computation wrapped in a MemoryArtifactsT monad transformer.
runMemoryArtifactsT :: MonadIO m =>
                       MemoryArtifactsT path m a
                    -- ^ The MemoryArtifactsT monad to execute.
                    -> m (a, Table path)
runMemoryArtifactsT s =
  do
    tab <- liftIO HashTable.new
    res <- runReaderT (unpackMemoryArtifactsT s) tab
    return (res, tab)

mapMemoryArtifactsT :: (Monad m, Monad n) =>
                    (m a -> n b) -> MemoryArtifactsT path m a ->
                    MemoryArtifactsT path n b
mapMemoryArtifactsT f = MemoryArtifactsT . mapReaderT f . unpackMemoryArtifactsT

artifact' :: (Hashable path, Eq path, MonadIO m) =>
             path -> Builder -> ReaderT (Table path) m (Maybe IOError)
artifact' path content =
  do
    tab <- ask
    liftIO (HashTable.insert tab path (toByteString content))
    return Nothing

artifactBytestring' :: (Hashable path, Eq path, MonadIO m) =>
                       path -> Strict.ByteString ->
                       ReaderT (Table path) m (Maybe IOError)
artifactBytestring' path content =
  do
    tab <- ask
    liftIO (HashTable.insert tab path content)
    return Nothing

artifactLazyBytestring' :: (Hashable path, Eq path, MonadIO m) =>
                           path -> Lazy.ByteString ->
                           ReaderT (Table path) m (Maybe IOError)
artifactLazyBytestring' path content =
  do
    tab <- ask
    liftIO (HashTable.insert tab path (Lazy.toStrict content))
    return Nothing

instance Monad m => Monad (MemoryArtifactsT path m) where
  return = MemoryArtifactsT . return
  s >>= f = MemoryArtifactsT $ unpackMemoryArtifactsT s >>=
                               unpackMemoryArtifactsT . f

instance Monad m => Applicative (MemoryArtifactsT path m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) =>
         Alternative (MemoryArtifactsT path m) where
  empty = lift empty
  s1 <|> s2 =
    MemoryArtifactsT (unpackMemoryArtifactsT s1 <|> unpackMemoryArtifactsT s2)

instance Functor (MemoryArtifactsT path m) where
  fmap = fmap

instance MonadIO m => MonadIO (MemoryArtifactsT path m) where
  liftIO = MemoryArtifactsT . liftIO

instance MonadTrans (MemoryArtifactsT path) where
  lift = MemoryArtifactsT . lift

instance (MonadIO m, Hashable path, Eq path) =>
         MonadArtifacts path (MemoryArtifactsT path m) where
  artifact path = MemoryArtifactsT . artifact' path
  artifactBytestring path = MemoryArtifactsT . artifactBytestring' path
  artifactLazyBytestring path = MemoryArtifactsT . artifactLazyBytestring' path

instance MonadCommentBuffer m =>
         MonadCommentBuffer (MemoryArtifactsT path m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (MemoryArtifactsT path m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (MemoryArtifactsT path m) where
  callCC f =
    MemoryArtifactsT (callCC (\c -> unpackMemoryArtifactsT (f (MemoryArtifactsT . c))))

instance (MonadError e m) => MonadError e (MemoryArtifactsT path m) where
  throwError = lift . throwError
  m `catchError` h =
    MemoryArtifactsT (unpackMemoryArtifactsT m `catchError`
                      (unpackMemoryArtifactsT . h))

instance MonadGenpos m => MonadGenpos (MemoryArtifactsT path m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (MemoryArtifactsT path m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadKeywords p t m =>
         MonadKeywords p t (MemoryArtifactsT path m) where
  mkKeyword p = lift . mkKeyword p

instance MonadLoader path info m =>
         MonadLoader path info (MemoryArtifactsT p m) where
  load = lift . load

instance MonadMessages msg m =>
         MonadMessages msg (MemoryArtifactsT path m) where
  message = lift . message

instance MonadPositions m => MonadPositions (MemoryArtifactsT path m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadSourceFiles m =>
         MonadSourceFiles (MemoryArtifactsT path m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m =>
         MonadSourceBuffer (MemoryArtifactsT path m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (MemoryArtifactsT path m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (MemoryArtifactsT path m) where
  ask = lift ask
  local f = mapMemoryArtifactsT (local f)

instance MonadSymbols m => MonadSymbols (MemoryArtifactsT path m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadWriter w m => MonadWriter w (MemoryArtifactsT path m) where
  tell = lift . tell
  listen = mapMemoryArtifactsT listen
  pass = mapMemoryArtifactsT pass

instance MonadPlus m => MonadPlus (MemoryArtifactsT path m) where
  mzero = lift mzero
  mplus s1 s2 = MemoryArtifactsT (mplus (unpackMemoryArtifactsT s1)
                                     (unpackMemoryArtifactsT s2))

instance MonadFix m => MonadFix (MemoryArtifactsT path m) where
  mfix f = MemoryArtifactsT (mfix (unpackMemoryArtifactsT . f))
