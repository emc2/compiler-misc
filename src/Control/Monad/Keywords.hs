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

module Control.Monad.Keywords(
       MonadKeywords(..),
       KeywordsT,
       Keywords,
       runKeywordsT,
       runKeywords,
       mapKeywordsT,
       keywordOrToken
       ) where

import Control.Applicative
import Control.Monad.Artifacts.Class
import Control.Monad.CommentBuffer.Class
import Control.Monad.Comments.Class
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Genpos.Class
import Control.Monad.Gensym.Class
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
import Data.ByteString hiding (empty)
import Data.HashTable.IO(BasicHashTable)

import qualified Data.HashTable.IO as HashTable

type Table tok pos = BasicHashTable ByteString (pos -> tok)

newtype KeywordsT pos tok m a =
  KeywordsT { unpackKeywordsT :: (ReaderT (Table tok pos) m) a }

type Keywords pos tok a = KeywordsT pos tok IO a

-- | Execute the computation represented by a Keywords monad.
runKeywords :: Keywords pos tok a
           -- ^ The low and high range of the symbols.
           -> [(ByteString, pos -> tok)]
           -- ^ The mapping of symbols.  The mapping to the lowest
           -- index is taken as the null symbol.
           -> IO a
runKeywords = runKeywordsT

-- | Execute the computation wrapped in a KeywordsT monad transformer.
runKeywordsT :: MonadIO m =>
               KeywordsT pos tok m a
            -- ^ The KeywordsT monad to execute.
            -> [(ByteString, pos -> tok)]
            -- ^ The mapping of symbols to indexes.  The mapping to the
            -- lowest index is taken as the null symbol.
            -> m a
runKeywordsT s keywords =
  do
    tab <- liftIO (HashTable.fromList keywords)
    runReaderT (unpackKeywordsT s) tab

mapKeywordsT :: (Monad m, Monad n) =>
                (m a -> n b) -> KeywordsT t p m a -> KeywordsT t p n b
mapKeywordsT f = KeywordsT . mapReaderT f . unpackKeywordsT

mkKeyword' :: MonadIO m => ByteString -> pos ->
              (ReaderT (Table tok pos) m) (Maybe tok)
mkKeyword' text pos =
  do
    tab <- ask
    entry <- liftIO (HashTable.lookup tab text)
    case entry of
      Just func -> return $! Just $! func pos
      Nothing -> return Nothing

instance Monad m => Monad (KeywordsT p t m) where
  return = KeywordsT . return
  s >>= f = KeywordsT $ unpackKeywordsT s >>= unpackKeywordsT . f

instance Monad m => Applicative (KeywordsT p t m) where
  pure = return
  (<*>) = ap

instance (Monad m, Alternative m) => Alternative (KeywordsT p t m) where
  empty = lift empty
  s1 <|> s2 = KeywordsT (unpackKeywordsT s1 <|> unpackKeywordsT s2)

instance Functor (KeywordsT p t m) where
  fmap = fmap

instance MonadIO m => MonadIO (KeywordsT p t m) where
  liftIO = KeywordsT . liftIO

instance MonadTrans (KeywordsT p t) where
  lift = KeywordsT . lift

instance MonadIO m => MonadKeywords pos tok (KeywordsT pos tok m) where
  mkKeyword pos = KeywordsT .  mkKeyword' pos

instance MonadArtifacts path m => MonadArtifacts path (KeywordsT p t m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCommentBuffer m => MonadCommentBuffer (KeywordsT p t m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (KeywordsT p t m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (KeywordsT p t m) where
  callCC f = KeywordsT (callCC (\c -> unpackKeywordsT (f (KeywordsT . c))))

instance (MonadError e m) => MonadError e (KeywordsT p t m) where
  throwError = lift . throwError
  m `catchError` h =
    KeywordsT (unpackKeywordsT m `catchError` (unpackKeywordsT . h))

instance MonadGenpos m => MonadGenpos (KeywordsT p t m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (KeywordsT p t m) where
  symbol = lift . symbol
  unique = lift . unique

instance (Monoid w, MonadJournal w m) => MonadJournal w (KeywordsT p t m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadMessages msg m => MonadMessages msg (KeywordsT p t m) where
  message = lift . message

instance MonadLoader path info m =>
         MonadLoader path info (KeywordsT p t m) where
  load = lift . load

instance MonadPositions m => MonadPositions (KeywordsT p t m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadSourceFiles m => MonadSourceFiles (KeywordsT p t m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m => MonadSourceBuffer (KeywordsT p t m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (KeywordsT p t m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (KeywordsT p t m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadReader r m => MonadReader r (KeywordsT p t m) where
  ask = lift ask
  local f = mapKeywordsT (local f)

instance MonadWriter w m => MonadWriter w (KeywordsT p t m) where
  tell = lift . tell
  listen = mapKeywordsT listen
  pass = mapKeywordsT pass

instance MonadPlus m => MonadPlus (KeywordsT p t m) where
  mzero = lift mzero
  mplus s1 s2 = KeywordsT (mplus (unpackKeywordsT s1) (unpackKeywordsT s2))

instance MonadFix m => MonadFix (KeywordsT p t m) where
  mfix f = KeywordsT (mfix (unpackKeywordsT . f))
