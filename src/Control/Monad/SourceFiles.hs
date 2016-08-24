-- Copyright (c) 2016 Eric McCorkle.  All rights reserved.
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

module Control.Monad.SourceFiles(
       MonadSourceFiles(..),
       SourceFilesT,
       SourceFiles,
       runSourceFilesT,
       runSourceFiles
       ) where

import Control.Applicative
import Control.Monad.Artifacts.Class
import Control.Monad.CommentBuffer.Class
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
import Control.Monad.State
import Control.Monad.Symbols.Class
import Data.Array
import Data.ByteString(ByteString)
import Data.Position.Filename

type Table = Array Filename (Array Word ByteString)

newtype SourceFilesT m a =
  SourceFilesT { unpackSourceFilesT :: ReaderT Table m a }

type SourceFiles = SourceFilesT IO

-- | Execute the computation represented by a SourceFiles monad.
runSourceFiles :: SourceFiles a
               -- ^ The SourceFilesT monad to execute.
               -> Table
               -- ^ A hash table containing the contents of all files.
               -> IO a
runSourceFiles = runSourceFilesT

-- | Execute the computation wrapped in a SourceFilesT monad transformer.
runSourceFilesT :: MonadIO m =>
                   SourceFilesT m a
                -- ^ The SourceFilesT monad to execute.
                -> Table
                -- ^ A hash table containing the contents of all files.
                -> m a
runSourceFilesT s = runReaderT (unpackSourceFilesT s)

sourceFile' :: MonadIO m =>
               Filename -> ReaderT Table m (Array Word ByteString)
sourceFile' fname =
  do
    tab <- ask
    return $! tab ! fname

instance Monad m => Monad (SourceFilesT m) where
  return = SourceFilesT . return
  s >>= f = SourceFilesT $ unpackSourceFilesT s >>= unpackSourceFilesT . f

instance Monad m => Applicative (SourceFilesT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) => Alternative (SourceFilesT m) where
  empty = lift empty
  s1 <|> s2 = SourceFilesT (unpackSourceFilesT s1 <|> unpackSourceFilesT s2)

instance Functor (SourceFilesT m) where
  fmap = fmap

instance (MonadIO m, MonadSourceFiles m) =>
         MonadSourceFiles (SourceFilesT m) where
  sourceFile = SourceFilesT . sourceFile'

instance MonadIO m => MonadIO (SourceFilesT m) where
  liftIO = SourceFilesT . liftIO

instance MonadTrans SourceFilesT where
  lift = SourceFilesT . lift

instance MonadArtifacts path m => MonadArtifacts path (SourceFilesT m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCommentBuffer m => MonadCommentBuffer (SourceFilesT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (SourceFilesT m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (SourceFilesT m) where
  callCC f = SourceFilesT (callCC (\c -> unpackSourceFilesT (f (SourceFilesT . c))))

instance (MonadError e m) => MonadError e (SourceFilesT m) where
  throwError = lift . throwError
  m `catchError` h =
    SourceFilesT (unpackSourceFilesT m `catchError` (unpackSourceFilesT . h))

instance MonadEdgeBuilder nodety m =>
         MonadEdgeBuilder nodety (SourceFilesT m) where
  addEdge src dst = lift . addEdge src dst

instance MonadGenpos m => MonadGenpos (SourceFilesT m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (SourceFilesT m) where
  symbol = lift . symbol
  unique = lift . unique

instance (Monoid w, MonadJournal w m) => MonadJournal w (SourceFilesT m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m => MonadKeywords p t (SourceFilesT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadLoader path info m => MonadLoader path info (SourceFilesT m) where
  load = lift . load

instance MonadMessages msg m => MonadMessages msg (SourceFilesT m) where
  message = lift . message

instance MonadNodeBuilder nodety m =>
         MonadNodeBuilder nodety (SourceFilesT m) where
  addNode = lift . addNode

instance MonadPositions m => MonadPositions (SourceFilesT m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadScopeStack m => MonadScopeStack (SourceFilesT m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance MonadScopeBuilder tmpscope m =>
         MonadScopeBuilder tmpscope (SourceFilesT m) where
  getScope = lift getScope
  setScope = lift . setScope

instance MonadState s m => MonadState s (SourceFilesT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (SourceFilesT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadPlus m => MonadPlus (SourceFilesT m) where
  mzero = lift mzero
  mplus s1 s2 = SourceFilesT (mplus (unpackSourceFilesT s1)
                                    (unpackSourceFilesT s2))

instance MonadFix m => MonadFix (SourceFilesT m) where
  mfix f = SourceFilesT (mfix (unpackSourceFilesT . f))
