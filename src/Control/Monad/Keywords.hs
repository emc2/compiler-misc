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
       mapKeywordsT
       ) where

import Control.Applicative
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
import Control.Monad.Writer
import Data.ByteString hiding (empty)
import Data.Position
import Data.HashTable.IO(BasicHashTable)

import qualified Data.HashTable.IO as HashTable

type TokenMaker tok = (Position, Position) -> tok

type Table tok = BasicHashTable ByteString ((Position, Position) -> tok)

newtype KeywordsT tok m a =
  KeywordsT { unpackKeywordsT :: (ReaderT (Table tok) m) a }

type Keywords tok a = KeywordsT tok IO a

-- | Execute the computation represented by a Keywords monad.
runKeywords :: Keywords tok a
           -- ^ The low and high range of the symbols.
           -> [(ByteString, TokenMaker tok)]
           -- ^ The mapping of symbols.  The mapping to the lowest
           -- index is taken as the null symbol.
           -> IO a
runKeywords = runKeywordsT

-- | Execute the computation wrapped in a KeywordsT monad transformer.
runKeywordsT :: MonadIO m =>
               KeywordsT tok m a
            -- ^ The KeywordsT monad to execute.
            -> [(ByteString, TokenMaker tok)]
            -- ^ The mapping of symbols to indexes.  The mapping to the
            -- lowest index is taken as the null symbol.
            -> m a
runKeywordsT s keywords =
  do
    tab <- liftIO (HashTable.fromList keywords)
    runReaderT (unpackKeywordsT s) tab

mapKeywordsT :: (Monad m, Monad n) =>
                (m a -> n b) -> KeywordsT t m a -> KeywordsT t n b
mapKeywordsT f = KeywordsT . mapReaderT f . unpackKeywordsT

mkKeyword' :: MonadIO m => (Position, Position) -> ByteString ->
              (ReaderT (Table tok) m) (Maybe tok)
mkKeyword' pos text =
  do
    tab <- ask
    entry <- liftIO (HashTable.lookup tab text)
    case entry of
      Just func -> return $! Just $! func pos
      Nothing -> return Nothing

instance Monad m => Monad (KeywordsT t m) where
  return = KeywordsT . return
  s >>= f = KeywordsT $ unpackKeywordsT s >>= unpackKeywordsT . f

instance Monad m => Applicative (KeywordsT t m) where
  pure = return
  (<*>) = ap

instance (Monad m, Alternative m) => Alternative (KeywordsT t m) where
  empty = lift empty
  s1 <|> s2 = KeywordsT (unpackKeywordsT s1 <|> unpackKeywordsT s2)

instance Functor (KeywordsT t m) where
  fmap = fmap

instance MonadIO m => MonadIO (KeywordsT t m) where
  liftIO = KeywordsT . liftIO

instance MonadTrans (KeywordsT t) where
  lift = KeywordsT . lift

instance MonadIO m => MonadKeywords tok (KeywordsT tok m) where
  mkKeyword pos = KeywordsT .  mkKeyword' pos

instance MonadCommentBuffer m => MonadCommentBuffer (KeywordsT t m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadComments m => MonadComments (KeywordsT t m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (KeywordsT t m) where
  callCC f = KeywordsT (callCC (\c -> unpackKeywordsT (f (KeywordsT . c))))

instance (Error e, MonadError e m) => MonadError e (KeywordsT t m) where
  throwError = lift . throwError
  m `catchError` h =
    KeywordsT (unpackKeywordsT m `catchError` (unpackKeywordsT . h))

instance MonadGenpos m => MonadGenpos (KeywordsT t m) where
  position = lift . position

instance MonadGensym m => MonadGensym (KeywordsT t m) where
  symbol = lift . symbol

instance MonadMessages msg m => MonadMessages msg (KeywordsT t m) where
  message = lift . message
  messages = lift messages

instance MonadPositions m => MonadPositions (KeywordsT t m) where
  positionInfo = lift . positionInfo

instance MonadSourceFiles m => MonadSourceFiles (KeywordsT t m) where
  sourceFile = lift . sourceFile

instance MonadSourceLoader m => MonadSourceLoader (KeywordsT t m) where
  loadSourceFile = lift . loadSourceFile

instance MonadState s m => MonadState s (KeywordsT t m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (KeywordsT t m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadReader r m => MonadReader r (KeywordsT t m) where
  ask = lift ask
  local f = mapKeywordsT (local f)

instance MonadWriter w m => MonadWriter w (KeywordsT t m) where
  tell = lift . tell
  listen = mapKeywordsT listen
  pass = mapKeywordsT pass

instance MonadPlus m => MonadPlus (KeywordsT t m) where
  mzero = lift mzero
  mplus s1 s2 = KeywordsT (mplus (unpackKeywordsT s1) (unpackKeywordsT s2))

instance MonadFix m => MonadFix (KeywordsT t m) where
  mfix f = KeywordsT (mfix (unpackKeywordsT . f))
