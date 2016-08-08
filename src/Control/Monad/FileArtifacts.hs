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

-- | Provides a 'MonadArtifacts' instance that simply reports the
-- contents of an in-memory table.  This is primarily intended for
-- testing, to allow a compiler using the 'MonadArtifacts' framework to
-- be tested without ever using the filesystem.
module Control.Monad.FileArtifacts(
       MonadArtifacts(..),
       FileArtifactsT,
       FileArtifacts,
       runFileArtifactsT,
       runFileArtifacts,
       mapFileArtifactsT
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
import System.Directory
import System.FilePath
import System.IO.Error

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy as Lazy

newtype FileArtifactsT m a =
  FileArtifactsT { unpackFileArtifactsT :: ReaderT Strict.ByteString m a }

type FileArtifacts = FileArtifactsT IO

-- | Execute the computation represented by a FileArtifacts monad.
runFileArtifacts :: FileArtifacts a
                 -- ^ The FileArtifactsT monad to execute.
                 -> Strict.ByteString
                 -- ^ The base directory to use.
                 -> IO a
runFileArtifacts = runFileArtifactsT

-- | Execute the computation wrapped in a FileArtifactsT monad transformer.
runFileArtifactsT :: MonadIO m =>
                     FileArtifactsT m a
                  -- ^ The FileArtifactsT monad to execute.
                  -> Strict.ByteString
                  -- ^ The base directory to use.
                  -> m a
runFileArtifactsT m = runReaderT (unpackFileArtifactsT m)

mapFileArtifactsT :: (Monad m, Monad n) =>
                    (m a -> n b) -> FileArtifactsT m a -> FileArtifactsT n b
mapFileArtifactsT f = FileArtifactsT . mapReaderT f . unpackFileArtifactsT

writeArtifact :: MonadIO m =>
                 (FilePath -> info -> IO ()) -> Strict.ByteString -> info ->
                 ReaderT Strict.ByteString m (Maybe IOError)
writeArtifact func path info =
  let
    strpath = Strict.toString path
    dirname = takeDirectory strpath

    writeFile' :: Strict.ByteString -> IO (Maybe IOError)
    writeFile' basedir =
      let
        strbase = Strict.toString basedir
        strdir = strbase ++ dirname
        strfile = strbase ++ strpath
      in do
        liftIO (createDirectoryIfMissing True strdir)
        liftIO (func strfile info)
        return Nothing
  in do
    basedir <- ask
    liftIO (writeFile' basedir `catchIOError` (\err -> return (Just err)))

artifact' :: MonadIO m =>
             Strict.ByteString -> Builder ->
             ReaderT Strict.ByteString m (Maybe IOError)
artifact' = writeArtifact (\fname -> toByteStringIO (Strict.writeFile fname))

artifactBytestring' :: MonadIO m =>
                       Strict.ByteString -> Strict.ByteString ->
                       ReaderT Strict.ByteString m (Maybe IOError)
artifactBytestring' = writeArtifact Strict.writeFile

artifactLazyBytestring' :: MonadIO m =>
                           Strict.ByteString -> Lazy.ByteString ->
                           ReaderT Strict.ByteString m (Maybe IOError)
artifactLazyBytestring' = writeArtifact Lazy.writeFile

instance Monad m => Monad (FileArtifactsT m) where
  return = FileArtifactsT . return
  s >>= f = FileArtifactsT $ unpackFileArtifactsT s >>= unpackFileArtifactsT . f

instance Monad m => Applicative (FileArtifactsT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) =>
         Alternative (FileArtifactsT m) where
  empty = lift empty
  s1 <|> s2 =
    FileArtifactsT (unpackFileArtifactsT s1 <|> unpackFileArtifactsT s2)

instance Functor (FileArtifactsT m) where
  fmap = fmap

instance MonadIO m => MonadIO (FileArtifactsT m) where
  liftIO = FileArtifactsT . liftIO

instance MonadTrans (FileArtifactsT) where
  lift = FileArtifactsT . lift

instance MonadIO m =>
         MonadArtifacts Strict.ByteString (FileArtifactsT m) where
  artifact path = FileArtifactsT . artifact' path
  artifactBytestring path = FileArtifactsT . artifactBytestring' path
  artifactLazyBytestring path = FileArtifactsT . artifactLazyBytestring' path

instance MonadCommentBuffer m =>
         MonadCommentBuffer (FileArtifactsT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (FileArtifactsT m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (FileArtifactsT m) where
  callCC f =
    FileArtifactsT (callCC (\c -> unpackFileArtifactsT (f (FileArtifactsT . c))))

instance MonadEdgeBuilder nodety m =>
         MonadEdgeBuilder nodety (FileArtifactsT m) where
  addEdge src dst = lift . addEdge src dst

instance (MonadError e m) => MonadError e (FileArtifactsT m) where
  throwError = lift . throwError
  m `catchError` h =
    FileArtifactsT (unpackFileArtifactsT m `catchError`
                    (unpackFileArtifactsT . h))

instance MonadGenpos m => MonadGenpos (FileArtifactsT m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (FileArtifactsT m) where
  symbol = lift . symbol
  unique = lift . unique

instance (Monoid w, MonadJournal w m) => MonadJournal w (FileArtifactsT m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m => MonadKeywords p t (FileArtifactsT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadLoader path info m =>
         MonadLoader path info (FileArtifactsT m) where
  load = lift . load

instance MonadMessages msg m => MonadMessages msg (FileArtifactsT m) where
  message = lift . message

instance MonadNodeBuilder nodety m =>
         MonadNodeBuilder nodety (FileArtifactsT m) where
  addNode = lift . addNode

instance MonadPositions m => MonadPositions (FileArtifactsT m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadSourceFiles m =>
         MonadSourceFiles (FileArtifactsT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m =>
         MonadSourceBuffer (FileArtifactsT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (FileArtifactsT m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (FileArtifactsT m) where
  ask = lift ask
  local f = mapFileArtifactsT (local f)

instance MonadSymbols m => MonadSymbols (FileArtifactsT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadWriter w m => MonadWriter w (FileArtifactsT m) where
  tell = lift . tell
  listen = mapFileArtifactsT listen
  pass = mapFileArtifactsT pass

instance MonadPlus m => MonadPlus (FileArtifactsT m) where
  mzero = lift mzero
  mplus s1 s2 = FileArtifactsT (mplus (unpackFileArtifactsT s1)
                                     (unpackFileArtifactsT s2))

instance MonadFix m => MonadFix (FileArtifactsT m) where
  mfix f = FileArtifactsT (mfix (unpackFileArtifactsT . f))
