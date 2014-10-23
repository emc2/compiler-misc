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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             FlexibleInstances, UndecidableInstances #-}

module Control.Monad.SourceLoader(
       MonadSourceLoader(..),
       MonadSourceFiles(..),
       SourceLoaderT,
       SourceLoader,
       runSourceLoaderT,
       runSourceLoader,
       mapSourceLoaderT
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.CommentBuffer.Class
import Control.Monad.Comments.Class
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Genpos.Class
import Control.Monad.Gensym.Class
import Control.Monad.Positions.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Symbols.Class
import Control.Monad.Writer
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceLoader.Class
import Data.Array
import Data.ByteString.Lazy hiding (map, length, empty)
import Data.ByteString.Lazy.UTF8(lines)
import Data.HashTable.IO(BasicHashTable)
import Data.Word
import Prelude hiding (lines, readFile)
import System.FilePath
import System.IO.Error

import qualified Data.HashTable.IO as HashTable

data SourceData =
  SourceData {
    sourceFiles :: !(BasicHashTable FilePath (Array Word ByteString)),
    sourcePaths :: ![FilePath]
  }

newtype SourceLoaderT m a =
  SourceLoaderT { unpackSourceLoaderT :: (ReaderT SourceData m) a }

type SourceLoader = SourceLoaderT IO

-- | Execute the computation wrapped in a @SourceLoaderT@ monad
-- transformer.
runSourceLoader :: SourceLoader a
                -- ^ The monad transformer to execute.
                -> [FilePath]
                -- ^ The source paths for loading source files.
                -> IO a
runSourceLoader = runSourceLoaderT

-- | Execute the computation wrapped in a @SourceLoaderT@ monad
-- transformer.
runSourceLoaderT :: MonadIO m =>
                    SourceLoaderT m a
                 -- ^ The monad transformer to execute.
                 -> [FilePath]
                 -- ^ The source paths for loading source files.
                 -> m a
runSourceLoaderT s paths =
  do
    tab <- liftIO HashTable.new
    runReaderT (unpackSourceLoaderT s)
               SourceData { sourceFiles = tab, sourcePaths = paths }

mapSourceLoaderT :: (Monad m, Monad n) =>
                    (m a -> n b) -> SourceLoaderT m a -> SourceLoaderT n b
mapSourceLoaderT f = SourceLoaderT . mapReaderT f . unpackSourceLoaderT

loadSourceFile' :: MonadIO m => FilePath ->
                   (ReaderT SourceData m) [ByteString]
loadSourceFile' filepath =
  do
    SourceData { sourcePaths = prefixes, sourceFiles = tab } <- ask
    -- Check the cache
    entry <- liftIO (HashTable.lookup tab filepath)
    case entry of
      -- If we already have the source, then just return it
      Just out -> return (elems out)
      -- Otherwise, try looking for the file using the source paths
      Nothing ->
        let
          -- Try to load the file, using the given source path
          tryLoad :: FilePath -> IO (Maybe [ByteString])
          tryLoad prefix =
            let
              -- Add the source path on to the file path
              combined = combine prefix filepath

              doLoad :: IO [ByteString]
              doLoad =
                do
                  -- Pull in the contents, split them up by lines
                  contents <- liftM lines
                                    (readFile combined)
                  -- Add the contents to the cache
                  HashTable.insert tab filepath
                    (listArray (1, fromIntegral (length contents)) contents)
                  return contents
            in
              liftM Just doLoad
                `catch`
                  (\e -> if isDoesNotExistError e
                            then return Nothing
                            else throw e)

          -- Go through the source paths, take the first one where the
          -- file exists
          loadFirst [] = return Nothing
          loadFirst (first : rest) =
            do
              res <- tryLoad first
              case res of
                Just out -> return (Just out)
                Nothing -> loadFirst rest
        in do
          res <- liftIO (loadFirst prefixes)
          case res of
            Just out -> return out
            Nothing ->
              throw (mkIOError doesNotExistErrorType
                               "Cannot locate source file"
                               Nothing (Just filepath))

instance Monad m => Monad (SourceLoaderT m) where
  return = SourceLoaderT . return
  s >>= f = SourceLoaderT $ unpackSourceLoaderT s >>= unpackSourceLoaderT . f

instance Monad m => Applicative (SourceLoaderT m) where
  pure = return
  (<*>) = ap

instance (Monad m, Alternative m) => Alternative (SourceLoaderT m) where
  empty = lift empty
  s1 <|> s2 = SourceLoaderT (unpackSourceLoaderT s1 <|> unpackSourceLoaderT s2)

instance Functor (SourceLoaderT m) where
  fmap = fmap

instance MonadIO m => MonadSourceLoader (SourceLoaderT m) where
  loadSourceFile = SourceLoaderT . loadSourceFile'

instance MonadIO m => MonadSourceFiles (SourceLoaderT m) where
  sourceLines = SourceLoaderT . loadSourceFile'

instance MonadIO m => MonadIO (SourceLoaderT m) where
  liftIO = SourceLoaderT . liftIO

instance MonadTrans SourceLoaderT where
  lift = SourceLoaderT . lift

instance MonadCommentBuffer m => MonadCommentBuffer (SourceLoaderT m) where
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

instance MonadGenpos m => MonadGenpos (SourceLoaderT m) where
  position fname line = lift . position fname line

instance MonadGensym m => MonadGensym (SourceLoaderT m) where
  symbol = SourceLoaderT . symbol

instance (Error e, MonadError e m) => MonadError e (SourceLoaderT m) where
  throwError = lift . throwError
  m `catchError` h =
    SourceLoaderT (unpackSourceLoaderT m `catchError` (unpackSourceLoaderT . h))

instance MonadPositions m => MonadPositions (SourceLoaderT m) where
  positionInfo = lift . positionInfo

instance MonadState s m => MonadState s (SourceLoaderT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (SourceLoaderT m) where
  nullSym = SourceLoaderT nullSym
  allNames = SourceLoaderT allNames
  allSyms = SourceLoaderT allSyms
  name = SourceLoaderT . name

instance MonadReader r m => MonadReader r (SourceLoaderT m) where
  ask = lift ask
  local f = mapSourceLoaderT (local f)

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
