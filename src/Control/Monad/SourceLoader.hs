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

-- | Provides a 'MonadLoader' instance that accesses the filesystem.
-- Note: this does not do any caching.
module Control.Monad.SourceLoader(
       MonadLoader(..),
       SourceLoaderT,
       SourceLoader,
       runSourceLoaderT,
       runSourceLoader,
       mapSourceLoaderT
       ) where

import Control.Applicative
import Control.Monad.CommentBuffer.Class
import Control.Monad.Comments.Class
import Control.Monad.Cont
import Control.Monad.Error
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
import System.IO.Error

import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy as Lazy

newtype SourceLoaderT m a =
  SourceLoaderT { unpackSourceLoaderT :: ReaderT [Strict.ByteString] m a }

type SourceLoader = SourceLoaderT IO

-- | Execute the computation represented by a SourceLoader monad.
runSourceLoader :: SourceLoader a
               -- ^ The SourceLoaderT monad to execute.
               -> [Strict.ByteString]
               -- ^ The search paths at which to search for files.
               -> IO a
runSourceLoader = runSourceLoaderT

-- | Execute the computation wrapped in a SourceLoaderT monad transformer.
runSourceLoaderT :: MonadIO m =>
                    SourceLoaderT m a
                -- ^ The SourceLoaderT monad to execute.
                -> [Strict.ByteString]
                -- ^ A hash table containing the contents of all files.
                -> m a
runSourceLoaderT s = runReaderT (unpackSourceLoaderT s)

mapSourceLoaderT :: (Monad m, Monad n) =>
                    (m a -> n b) -> SourceLoaderT m a -> SourceLoaderT n b
mapSourceLoaderT f = SourceLoaderT . mapReaderT f . unpackSourceLoaderT

load' :: MonadIO m =>
         Strict.ByteString -> ReaderT [Strict.ByteString] m Lazy.ByteString
load' path =
  let
    fpath = Strict.toString path

    -- Try each path in the prefixes, continue until something other
    -- than doesNotExistError happens.
    loadPrefixes :: [Strict.ByteString] -> IO Lazy.ByteString
    -- Try each prefix path in turn.
    loadPrefixes (first : rest) =
      let
        fullpath = Strict.toString first ++ fpath
      in
        Lazy.readFile fullpath `catchIOError`
          \err -> if isDoesNotExistError err
                    -- If we get a doesNotExistError, then continue.
                    then loadPrefixes rest
                    -- Otherwise, rethrow it.
                    else liftIO (ioError err)
    -- If we get to the end of the prefixes, and we have an error, throw it
    loadPrefixes [] = liftIO (ioError (mkIOError doesNotExistErrorType ""
                                                 Nothing (Just fpath)))
  in do
    prefixes <- ask
    liftIO (loadPrefixes prefixes)

instance Monad m => Monad (SourceLoaderT m) where
  return = SourceLoaderT . return
  s >>= f = SourceLoaderT $ unpackSourceLoaderT s >>= unpackSourceLoaderT . f

instance Monad m => Applicative (SourceLoaderT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) =>
         Alternative (SourceLoaderT m) where
  empty = lift empty
  s1 <|> s2 =
    SourceLoaderT (unpackSourceLoaderT s1 <|> unpackSourceLoaderT s2)

instance Functor (SourceLoaderT m) where
  fmap = fmap

instance MonadIO m => MonadIO (SourceLoaderT m) where
  liftIO = SourceLoaderT . liftIO

instance MonadTrans (SourceLoaderT) where
  lift = SourceLoaderT . lift

instance MonadIO m =>
         MonadLoader Strict.ByteString Lazy.ByteString (SourceLoaderT m) where
  load = SourceLoaderT . load'

instance MonadCommentBuffer m =>
         MonadCommentBuffer (SourceLoaderT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (SourceLoaderT m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (SourceLoaderT m) where
  callCC f =
    SourceLoaderT (callCC (\c -> unpackSourceLoaderT (f (SourceLoaderT . c))))

instance (Error e, MonadError e m) =>
         MonadError e (SourceLoaderT m) where
  throwError = lift . throwError
  m `catchError` h =
    SourceLoaderT (unpackSourceLoaderT m `catchError` (unpackSourceLoaderT . h))

instance MonadGenpos m => MonadGenpos (SourceLoaderT m) where
  position = lift . position

instance MonadGensym m => MonadGensym (SourceLoaderT m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadKeywords t m => MonadKeywords t (SourceLoaderT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadMessages msg m => MonadMessages msg (SourceLoaderT m) where
  message = lift . message

instance MonadPositions m => MonadPositions (SourceLoaderT m) where
  positionInfo = lift . positionInfo

instance MonadSourceFiles m =>
         MonadSourceFiles (SourceLoaderT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m =>
         MonadSourceBuffer (SourceLoaderT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (SourceLoaderT m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (SourceLoaderT m) where
  ask = lift ask
  local f = mapSourceLoaderT (local f)

instance MonadSymbols m => MonadSymbols (SourceLoaderT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadWriter w m => MonadWriter w (SourceLoaderT m) where
  tell = lift . tell
  listen = mapSourceLoaderT listen
  pass = mapSourceLoaderT pass

instance MonadPlus m => MonadPlus (SourceLoaderT m) where
  mzero = lift mzero
  mplus s1 s2 = SourceLoaderT (mplus (unpackSourceLoaderT s1)
                                     (unpackSourceLoaderT s2))

instance MonadFix m => MonadFix (SourceLoaderT m) where
  mfix f = SourceLoaderT (mfix (unpackSourceLoaderT . f))
