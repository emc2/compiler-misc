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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             FlexibleInstances, UndecidableInstances #-}

module Control.Monad.CommentBuffer(
       MonadCommentBuffer(..),
       CommentBufferT,
       CommentBuffer,
       runCommentBufferT,
       runCommentBuffer
       ) where

import Control.Applicative
import Control.Monad.Artifacts.Class
import Control.Monad.CommentBuffer.Class
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
import Control.Monad.ScopeBuilder.Class
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceBuffer.Class
import Control.Monad.State
import Control.Monad.Symbols.Class
import Data.ByteString hiding (reverse, empty)
import Data.Position.Point

import qualified Data.ByteString.Lazy as Lazy

data BufferState =
  BufferState {
    partialComment :: ![Lazy.ByteString],
    fullComments :: ![ByteString],
    savedComments :: ![(Point, [ByteString])]
  }

newtype CommentBufferT m a =
  CommentBufferT { unpackCommentBufferT :: (StateT BufferState m) a }

type CommentBuffer = CommentBufferT IO

-- | Execute the computation wrapped in a CommentBufferT monad tranformer.
runCommentBufferT :: Monad m =>
                     CommentBufferT m a
                  -- ^ The CommentBufferT monad transformer to execute.
                  -> m a
runCommentBufferT CommentBufferT { unpackCommentBufferT = c } =
  let
    initial = BufferState { partialComment = [],
                            fullComments = [],
                            savedComments = [] }
  in do
    (out, _) <- runStateT c initial
    return out

-- | Execute the computation wrapped in a CommentBuffer monad.
runCommentBuffer :: CommentBuffer a
                 -- ^ The CommentBufferT monad transformer to execute.
                 -> IO a
runCommentBuffer = runCommentBufferT

startComment' :: Monad m => (StateT BufferState m) ()
startComment' = return ()

appendComment' :: Monad m => Lazy.ByteString -> (StateT BufferState m) ()
appendComment' text =
  do
    s @ BufferState { partialComment = curr } <- get
    put s { partialComment = text : curr }

finishComment' :: Monad m => (StateT BufferState m) ()
finishComment' =
  do
    s @ BufferState { partialComment = partial, fullComments = full } <- get
    put s { fullComments = Lazy.toStrict (Lazy.concat (reverse partial)) : full,
            partialComment = [] }

addComment' :: Monad m => Lazy.ByteString -> (StateT BufferState m) ()
addComment' text =
  do
    s @ BufferState { fullComments = full } <- get
    put s { fullComments = Lazy.toStrict text : full }

saveCommentsAsPreceeding' :: Monad m => Point -> (StateT BufferState m) ()
saveCommentsAsPreceeding' pos =
  do
    s @ BufferState { fullComments = full, savedComments = saved } <- get
    put s { savedComments = (pos, full) : saved }

clearComments' :: Monad m => (StateT BufferState m) ()
clearComments' =
  do
    s <- get
    put s { fullComments = [] }

instance Monad m => Monad (CommentBufferT m) where
  return = CommentBufferT . return
  s >>= f = CommentBufferT $ unpackCommentBufferT s >>= unpackCommentBufferT . f

instance Monad m => Applicative (CommentBufferT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) => Alternative (CommentBufferT m) where
  empty = lift empty
  s1 <|> s2 =
    CommentBufferT (unpackCommentBufferT s1 <|> unpackCommentBufferT s2)

instance Functor (CommentBufferT m) where
  fmap = fmap

instance MonadIO m => MonadIO (CommentBufferT m) where
  liftIO = CommentBufferT . liftIO

instance MonadTrans CommentBufferT where
  lift = CommentBufferT . lift

instance Monad m => MonadCommentBuffer (CommentBufferT m) where
  startComment = CommentBufferT startComment'
  appendComment = CommentBufferT . appendComment'
  finishComment = CommentBufferT finishComment'
  addComment = CommentBufferT . addComment'
  saveCommentsAsPreceeding = CommentBufferT . saveCommentsAsPreceeding'
  clearComments = CommentBufferT clearComments'

instance MonadArtifacts path m => MonadArtifacts path (CommentBufferT m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCont m => MonadCont (CommentBufferT m) where
  callCC f = CommentBufferT
    (callCC (\c -> unpackCommentBufferT (f (CommentBufferT . c))))

instance MonadEdgeBuilder nodety m =>
         MonadEdgeBuilder nodety (CommentBufferT m) where
  addEdge src dst = lift . addEdge src dst

instance (MonadError e m) => MonadError e (CommentBufferT m) where
  throwError = lift . throwError
  m `catchError` h =
    CommentBufferT (unpackCommentBufferT m `catchError`
                    (unpackCommentBufferT . h))

instance MonadGenpos m => MonadGenpos (CommentBufferT m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (CommentBufferT m) where
  symbol = lift . symbol
  unique = lift . unique

instance (Monoid w, MonadJournal w m) => MonadJournal w (CommentBufferT m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m => MonadKeywords p t (CommentBufferT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadLoader path info m =>
         MonadLoader path info (CommentBufferT m) where
  load = lift . load

instance MonadMessages msg m => MonadMessages msg (CommentBufferT m) where
  message = lift . message

instance MonadNodeBuilder nodety m =>
         MonadNodeBuilder nodety (CommentBufferT m) where
  addNode = lift . addNode

instance MonadPositions m => MonadPositions (CommentBufferT m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadScopeStack m => MonadScopeStack (CommentBufferT m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance MonadScopeBuilder tmpscope m =>
         MonadScopeBuilder tmpscope (CommentBufferT m) where
  getScope = lift getScope
  setScope = lift . setScope

instance MonadSourceFiles m => MonadSourceFiles (CommentBufferT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m => MonadSourceBuffer (CommentBufferT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (CommentBufferT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (CommentBufferT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadPlus m => MonadPlus (CommentBufferT m) where
  mzero = lift mzero
  mplus s1 s2 =
    CommentBufferT (mplus (unpackCommentBufferT s1) (unpackCommentBufferT s2))

instance MonadFix m => MonadFix (CommentBufferT m) where
  mfix f = CommentBufferT (mfix (unpackCommentBufferT . f))
