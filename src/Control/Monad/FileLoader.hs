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

-- | Provides a 'MonadLoader' instance that accesses the filesystem.
-- Note: this does not do any caching.
module Control.Monad.FileLoader(
       MonadLoader(..),
       FileLoaderT,
       FileLoader,
       runFileLoaderT,
       runFileLoader,
       mapFileLoaderT
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
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceBuffer.Class
import Control.Monad.State
import Control.Monad.Symbols.Class
import Control.Monad.Writer
import Data.Position.Filename
import System.IO.Error

import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy as Lazy

newtype FileLoaderT m a =
  FileLoaderT { unpackFileLoaderT :: ReaderT [Strict.ByteString] m a }

type FileLoader = FileLoaderT IO

-- | Execute the computation represented by a FileLoader monad.
runFileLoader :: FileLoader a
              -- ^ The FileLoaderT monad to execute.
              -> [Strict.ByteString]
              -- ^ The search paths at which to search for files.
              -> IO a
runFileLoader = runFileLoaderT

-- | Execute the computation wrapped in a FileLoaderT monad transformer.
runFileLoaderT :: MonadIO m =>
                  FileLoaderT m a
               -- ^ The FileLoaderT monad to execute.
               -> [Strict.ByteString]
               -- ^ The search paths at which to search for files.
               -> m a
runFileLoaderT s = runReaderT (unpackFileLoaderT s)

mapFileLoaderT :: (Monad m, Monad n) =>
                   (m a -> n b) -> FileLoaderT m a -> FileLoaderT n b
mapFileLoaderT f = FileLoaderT . mapReaderT f . unpackFileLoaderT

load' :: (MonadIO m, MonadGenpos m) =>
         Strict.ByteString ->
         ReaderT [Strict.ByteString] m
                 (Either IOError (Filename, Lazy.ByteString))
load' path =
  let
    fpath = Strict.toString path

    -- Try each path in the prefixes, continue until something other
    -- than doesNotExistError happens.
    loadPrefixes :: (MonadIO m, MonadGenpos m) =>
                    [Strict.ByteString] ->
                    m (Either IOError (Filename, Lazy.ByteString))
    -- Try each prefix path in turn.
    loadPrefixes (first : rest) =
      let
        fullpath = Strict.toString first ++ fpath
      in do
        res <- liftIO $! tryIOError (Lazy.readFile fullpath)
        case res of
          -- If we get a doesNotExistError, then continue.
          Left err | isDoesNotExistError err -> loadPrefixes rest
                   | otherwise -> return (Left err)
          -- Otherwise, return what we got.
          Right contents ->
            do
              fname <- filename FileInfo { fileInfoName = path,
                                           fileInfoDir = first }
              return (Right (fname, contents))
    -- If we get to the end of the prefixes, and we have an error, throw it
    loadPrefixes [] = return $! Left $! mkIOError doesNotExistErrorType ""
                                                  Nothing (Just fpath)
  in do
    prefixes <- ask
    loadPrefixes prefixes

instance Monad m => Monad (FileLoaderT m) where
  return = FileLoaderT . return
  s >>= f = FileLoaderT $ unpackFileLoaderT s >>= unpackFileLoaderT . f

instance Monad m => Applicative (FileLoaderT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) =>
         Alternative (FileLoaderT m) where
  empty = lift empty
  s1 <|> s2 =
    FileLoaderT (unpackFileLoaderT s1 <|> unpackFileLoaderT s2)

instance Functor (FileLoaderT m) where
  fmap = fmap

instance MonadIO m => MonadIO (FileLoaderT m) where
  liftIO = FileLoaderT . liftIO

instance MonadTrans (FileLoaderT) where
  lift = FileLoaderT . lift

instance (MonadIO m, MonadGenpos m) =>
         MonadLoader Strict.ByteString Lazy.ByteString (FileLoaderT m) where
  load = FileLoaderT . load'

instance MonadArtifacts path m => MonadArtifacts path (FileLoaderT m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCommentBuffer m =>
         MonadCommentBuffer (FileLoaderT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (FileLoaderT m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (FileLoaderT m) where
  callCC f =
    FileLoaderT (callCC (\c -> unpackFileLoaderT (f (FileLoaderT . c))))

instance (MonadError e m) => MonadError e (FileLoaderT m) where
  throwError = lift . throwError
  m `catchError` h =
    FileLoaderT (unpackFileLoaderT m `catchError` (unpackFileLoaderT . h))

instance MonadEdgeBuilder nodety m =>
         MonadEdgeBuilder nodety (FileLoaderT m) where
  addEdge src dst = lift . addEdge src dst

instance MonadGenpos m => MonadGenpos (FileLoaderT m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (FileLoaderT m) where
  symbol = lift . symbol
  unique = lift . unique

instance (Monoid w, MonadJournal w m) => MonadJournal w (FileLoaderT m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m => MonadKeywords p t (FileLoaderT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadMessages msg m => MonadMessages msg (FileLoaderT m) where
  message = lift . message

instance MonadNodeBuilder nodety m =>
         MonadNodeBuilder nodety (FileLoaderT m) where
  addNode = lift . addNode

instance MonadPositions m => MonadPositions (FileLoaderT m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadSourceFiles m =>
         MonadSourceFiles (FileLoaderT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m =>
         MonadSourceBuffer (FileLoaderT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (FileLoaderT m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (FileLoaderT m) where
  ask = lift ask
  local f = mapFileLoaderT (local f)

instance MonadSymbols m => MonadSymbols (FileLoaderT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadWriter w m => MonadWriter w (FileLoaderT m) where
  tell = lift . tell
  listen = mapFileLoaderT listen
  pass = mapFileLoaderT pass

instance MonadPlus m => MonadPlus (FileLoaderT m) where
  mzero = lift mzero
  mplus s1 s2 = FileLoaderT (mplus (unpackFileLoaderT s1)
                                   (unpackFileLoaderT s2))

instance MonadFix m => MonadFix (FileLoaderT m) where
  mfix f = FileLoaderT (mfix (unpackFileLoaderT . f))
