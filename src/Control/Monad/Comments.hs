-- Copyright (c) 2016 Eric McCorkle.  All rights reserved.
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
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             FlexibleInstances, UndecidableInstances #-}

-- | Monad with access to preserved source comments.
--
-- Some compiler-related tools rely on specially-formatted comments in
-- their functioning.  An example of this is documentation-generation
-- tools like javadoc, haddock, and doxygen.  This monad provides a
-- way to implement this sort of functionality.
module Control.Monad.Comments(
       MonadComments(..),
       CommentsT,
       Comments,
       runCommentsT,
       runComments,
       mapCommentsT
       ) where

import Control.Applicative
import Control.Monad.Artifacts.Class
import Control.Monad.Comments.Class
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Genpos.Class
import Control.Monad.Gensym.Class
import Control.Monad.GraphBuilder.Class
import Control.Monad.Journal
import Control.Monad.Keywords.Class
import Control.Monad.Loader.Class
import Control.Monad.Messages.Class
import Control.Monad.Positions.Class
import Control.Monad.Reader
import Control.Monad.ScopeBuilder.Class
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceBuffer.Class
import Control.Monad.State
import Control.Monad.Symbols.Class
import Control.Monad.Writer
import Data.ByteString(ByteString)
import Data.HashTable.IO(BasicHashTable)
import Data.Maybe
import Data.Position.Point

import qualified Data.HashTable.IO as HashTable

-- | Monad transformer which provides access to saved comments.
newtype CommentsT m a =
  CommentsT {
      unpackCommentsT :: (ReaderT (BasicHashTable Point [ByteString]) m) a
    }

-- | Monad which provides access to saved comments.
type Comments = CommentsT IO

-- | Execute the computation wrapped in a CommentsT monad transformer.
runCommentsT :: Monad m =>
                CommentsT m a
             -> BasicHashTable Point [ByteString]
             -> m a
runCommentsT c = runReaderT (unpackCommentsT c)

-- | Execute the computation wrapped in a Comments monad.
runComments :: Comments a
            -> BasicHashTable Point [ByteString]
            -> IO a
runComments c = runReaderT (unpackCommentsT c)

mapCommentsT :: (Monad m, Monad n) =>
                (m a -> n b) -> CommentsT m a -> CommentsT n b
mapCommentsT f = CommentsT . mapReaderT f . unpackCommentsT

preceedingComments' :: MonadIO m => Point ->
                       (ReaderT (BasicHashTable Point [ByteString]) m)
                         [ByteString]
preceedingComments' pos =
  do
    tab <- ask
    entry <- liftIO (HashTable.lookup tab pos)
    return (fromMaybe [] entry)

instance Monad m => Monad (CommentsT m) where
  return = CommentsT . return
  s >>= f = CommentsT $ unpackCommentsT s >>= unpackCommentsT . f

instance Monad m => Applicative (CommentsT m) where
  pure = return
  (<*>) = ap

instance (Monad m, Alternative m) => Alternative (CommentsT m) where
  empty = lift empty
  s1 <|> s2 = CommentsT (unpackCommentsT s1 <|> unpackCommentsT s2)

instance Functor (CommentsT m) where
  fmap = fmap

instance MonadIO m => MonadIO (CommentsT m) where
  liftIO = CommentsT . liftIO

instance MonadTrans CommentsT where
  lift = CommentsT . lift

instance MonadIO m => MonadComments (CommentsT m) where
  preceedingComments = CommentsT . preceedingComments'

instance MonadArtifacts path m => MonadArtifacts path (CommentsT m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCont m => MonadCont (CommentsT m) where
  callCC f = CommentsT (callCC (\c -> unpackCommentsT (f (CommentsT . c))))

instance MonadEdgeBuilder nodety m =>
         MonadEdgeBuilder nodety (CommentsT m) where
  addEdge src dst = lift . addEdge src dst

instance (MonadError e m) => MonadError e (CommentsT m) where
  throwError = lift . throwError
  m `catchError` h =
    CommentsT (unpackCommentsT m `catchError` (unpackCommentsT . h))

instance MonadGenpos m => MonadGenpos (CommentsT m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (CommentsT m) where
  symbol = lift . symbol
  unique = lift . unique

instance (Monoid w, MonadJournal w m) => MonadJournal w (CommentsT m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m => MonadKeywords p t (CommentsT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadLoader path info m =>
         MonadLoader path info (CommentsT m) where
  load = lift . load

instance MonadMessages msg m => MonadMessages msg (CommentsT m) where
  message = lift . message

instance MonadNodeBuilder nodety m =>
         MonadNodeBuilder nodety (CommentsT m) where
  addNode = lift . addNode

instance MonadPositions m => MonadPositions (CommentsT m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadScopeStack m => MonadScopeStack (CommentsT m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance MonadScopeBuilder tmpscope m =>
         MonadScopeBuilder tmpscope (CommentsT m) where
  getScope = lift getScope
  setScope = lift . setScope

instance MonadSourceFiles m => MonadSourceFiles (CommentsT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m => MonadSourceBuffer (CommentsT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (CommentsT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (CommentsT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadReader r m => MonadReader r (CommentsT m) where
  ask = lift ask
  local f = mapCommentsT (local f)

instance MonadWriter w m => MonadWriter w (CommentsT m) where
  tell = lift . tell
  listen = mapCommentsT listen
  pass = mapCommentsT pass

instance MonadPlus m => MonadPlus (CommentsT m) where
  mzero = lift mzero
  mplus s1 s2 = CommentsT (mplus (unpackCommentsT s1) (unpackCommentsT s2))

instance MonadFix m => MonadFix (CommentsT m) where
  mfix f = CommentsT (mfix (unpackCommentsT . f))
