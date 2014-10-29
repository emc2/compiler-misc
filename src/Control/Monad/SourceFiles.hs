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
import Control.Exception
import Control.Monad.CommentBuffer.Class
import Control.Monad.Comments.Class
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Genpos.Class
import Control.Monad.Gensym.Class
import Control.Monad.Keywords.Class
import Control.Monad.Messages.Class
import Control.Monad.Positions.Class
import Control.Monad.Reader
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceLoader.Class
import Control.Monad.State
import Control.Monad.Symbols.Class
import Data.HashTable.IO(BasicHashTable)
import Data.Array
import Data.Word
import System.IO.Error

import qualified Data.HashTable.IO as HashTable
import qualified Data.ByteString.Lazy as Lazy

type Table = BasicHashTable FilePath (Array Word Lazy.ByteString)

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
               FilePath -> ReaderT Table m (Array Word Lazy.ByteString)
sourceFile' path =
  do
    tab <- ask
    res <- liftIO (HashTable.lookup tab path)
    case res of
      Just out -> return out
      Nothing -> throw (mkIOError doesNotExistErrorType
                                  "Cannot locate source file"
                                  Nothing (Just path))

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

instance (Error e, MonadError e m) => MonadError e (SourceFilesT m) where
  throwError = lift . throwError
  m `catchError` h =
    SourceFilesT (unpackSourceFilesT m `catchError` (unpackSourceFilesT . h))

instance MonadGenpos m => MonadGenpos (SourceFilesT m) where
  position = lift . position

instance MonadGensym m => MonadGensym (SourceFilesT m) where
  symbol = lift . symbol

instance MonadKeywords t m => MonadKeywords t (SourceFilesT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadMessages msg m => MonadMessages msg (SourceFilesT m) where
  message = lift . message

instance MonadPositions m => MonadPositions (SourceFilesT m) where
  positionInfo = lift . positionInfo

instance MonadSourceLoader m => MonadSourceLoader (SourceFilesT m) where
  loadSourceFile = lift . loadSourceFile

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
  mplus s1 s2 = SourceFilesT (mplus (unpackSourceFilesT s1) (unpackSourceFilesT s2))

instance MonadFix m => MonadFix (SourceFilesT m) where
  mfix f = SourceFilesT (mfix (unpackSourceFilesT . f))
