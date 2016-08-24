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

module Control.Monad.SourceBuffer(
       MonadSourceBuffer(..),
       MonadSourceFiles(..),
       SourceBufferT,
       SourceBuffer,
       runSourceBufferT,
       runSourceBuffer
       ) where

import Control.Applicative
import Control.Exception
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
import Control.Monad.State
import Control.Monad.Symbols.Class
import Control.Monad.ScopeBuilder.Class
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceBuffer.Class
import Data.Array
import Data.ByteString (ByteString)
import Data.HashTable.IO(BasicHashTable)
import Data.Int
import Data.Position.Filename
import Prelude hiding (lines, readFile)
import System.IO.Error

import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.HashTable.IO as HashTable

type Table = BasicHashTable Filename (Array Word ByteString)

data BufferState =
  BufferState {
    stFileName :: !Filename,
    stContents :: !Lazy.ByteString,
    stBuffer :: ![ByteString],
    stOffset :: !Int64
  }

newtype SourceBufferT m a =
  SourceBufferT {
    unpackSourceBufferT :: (StateT BufferState (ReaderT Table m)) a
  }

type SourceBuffer = SourceBufferT IO

-- | Execute the computation wrapped in a @SourceBufferT@ monad
-- transformer.
runSourceBuffer :: SourceBuffer a
                -- ^ The monad transformer to execute.
                -> IO a
runSourceBuffer = runSourceBufferT

-- | Execute the computation wrapped in a @SourceBufferT@ monad
-- transformer.
runSourceBufferT :: MonadIO m =>
                    SourceBufferT m a
                 -- ^ The monad transformer to execute.
                 -> m a
runSourceBufferT s =
  do
    tab <- liftIO HashTable.new
    (out, _) <- runReaderT (runStateT (unpackSourceBufferT s) undefined) tab
    return out

sourceFile' :: MonadIO m =>
               Filename ->
               (StateT BufferState (ReaderT Table m)) (Array Word ByteString)
sourceFile' path =
  do
    tab <- ask
    res <- liftIO (HashTable.lookup tab path)
    case res of
      Just out -> return out
      Nothing -> throw (mkIOError doesNotExistErrorType
                                  "Cannot locate source file"
                                  Nothing Nothing)

startFile' :: Monad m => Filename -> Lazy.ByteString ->
              (StateT BufferState (ReaderT Table m)) ()
startFile' fname fcontents =
  put BufferState { stFileName = fname, stContents = fcontents,
                    stBuffer = [], stOffset = 0 }

finishFile' :: MonadIO m => (StateT BufferState (ReaderT Table m)) ()
finishFile' =
  let
    finish :: Lazy.ByteString -> ByteString
    finish = Lazy.toStrict
  in do
    BufferState { stFileName = fname, stContents = remaining,
                  stBuffer = buf } <- get
    tab <- ask
    liftIO (HashTable.insert tab fname $!
                             listArray (1, fromIntegral (length buf) + 1)
                                       (reverse (finish remaining : buf)))

linebreak' :: Monad m => Int -> (StateT BufferState (ReaderT Table m)) ()
linebreak' intoff =
  let
    off = fromIntegral intoff
  in do
    st @ BufferState { stContents = contents, stOffset = oldoff,
                       stBuffer = buf } <- get
    put st { stContents = Lazy.drop (off - oldoff) contents,
             stBuffer = Lazy.toStrict (Lazy.take (off - oldoff - 1) contents) :
                        buf,
             stOffset = off }

instance Monad m => Monad (SourceBufferT m) where
  return = SourceBufferT . return
  s >>= f = SourceBufferT $ unpackSourceBufferT s >>= unpackSourceBufferT . f

instance Monad m => Applicative (SourceBufferT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) => Alternative (SourceBufferT m) where
  empty = lift empty
  s1 <|> s2 = SourceBufferT (unpackSourceBufferT s1 <|> unpackSourceBufferT s2)

instance Functor (SourceBufferT m) where
  fmap = fmap

instance MonadIO m => MonadSourceBuffer (SourceBufferT m) where
  startFile fname = SourceBufferT . startFile' fname
  finishFile = SourceBufferT finishFile'
  linebreak = SourceBufferT . linebreak'

instance MonadIO m => MonadSourceFiles (SourceBufferT m) where
  sourceFile = SourceBufferT . sourceFile'

instance MonadIO m => MonadIO (SourceBufferT m) where
  liftIO = SourceBufferT . liftIO

instance MonadTrans SourceBufferT where
  lift = SourceBufferT . lift . lift

instance MonadArtifacts path m => MonadArtifacts path (SourceBufferT m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCommentBuffer m => MonadCommentBuffer (SourceBufferT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (SourceBufferT m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (SourceBufferT m) where
  callCC f =
    SourceBufferT (callCC (\c -> unpackSourceBufferT (f (SourceBufferT . c))))

instance MonadGenpos m => MonadGenpos (SourceBufferT m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (SourceBufferT m) where
  symbol = SourceBufferT . symbol
  unique = lift . unique

instance (MonadError e m) => MonadError e (SourceBufferT m) where
  throwError = lift . throwError
  m `catchError` h =
    SourceBufferT (unpackSourceBufferT m `catchError` (unpackSourceBufferT . h))

instance MonadEdgeBuilder nodety m =>
         MonadEdgeBuilder nodety (SourceBufferT m) where
  addEdge src dst = lift . addEdge src dst

instance (Monoid w, MonadJournal w m) => MonadJournal w (SourceBufferT m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m => MonadKeywords p t (SourceBufferT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadLoader path info m =>
         MonadLoader path info (SourceBufferT m) where
  load = lift . load

instance MonadNodeBuilder nodety m =>
         MonadNodeBuilder nodety (SourceBufferT m) where
  addNode = lift . addNode

instance MonadMessages msg m => MonadMessages msg (SourceBufferT m) where
  message = lift . message

instance MonadPositions m => MonadPositions (SourceBufferT m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadScopeStack m => MonadScopeStack (SourceBufferT m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance MonadScopeBuilder tmpscope m =>
         MonadScopeBuilder tmpscope (SourceBufferT m) where
  alterScope = lift . alterScope

instance MonadState s m => MonadState s (SourceBufferT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (SourceBufferT m) where
  nullSym = SourceBufferT nullSym
  allNames = SourceBufferT allNames
  allSyms = SourceBufferT allSyms
  name = SourceBufferT . name

instance MonadPlus m => MonadPlus (SourceBufferT m) where
  mzero = lift mzero
  mplus s1 s2 = SourceBufferT (mplus (unpackSourceBufferT s1)
                                     (unpackSourceBufferT s2))

instance MonadFix m => MonadFix (SourceBufferT m) where
  mfix f = SourceBufferT (mfix (unpackSourceBufferT . f))
