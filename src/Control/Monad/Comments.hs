-- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
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

-- | Monad with access to preserved source comments.
--
-- Some compiler-related tools rely on specially-formatted comments in
-- their functioning.  An example of this is documentation-generation
-- tools like javadoc, haddock, and doxygen.  This monad provides a
-- way to implement this sort of functionality.
module Control.Monad.Comments(
       MonadComments(..),
       CommentsT,
       Comments,
       runCommentsT,
       runComments,
       mapCommentsT
       ) where

import Control.Applicative
import Control.Monad.Comments.Class
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Genpos.Class
import Control.Monad.Gensym.Class
import Control.Monad.Keywords.Class
import Control.Monad.Positions.Class
import Control.Monad.Reader
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceLoader.Class
import Control.Monad.State
import Control.Monad.Symbols.Class
import Control.Monad.Writer
import Data.ByteString(ByteString)
import Data.HashTable.IO(BasicHashTable)
import Data.Position

-- | Monad transformer which provides access to saved comments.
newtype CommentsT m a =
  CommentsT {
      unpackCommentsT :: (ReaderT (BasicHashTable Position [ByteString]) m) a
    }

-- | Monad which provides access to saved comments.
type Comments = CommentsT IO

-- | Execute the computation wrapped in a CommentsT monad transformer.
runCommentsT :: Monad m =>
                CommentsT m a
             -> BasicHashTable Position [ByteString]
             -> m a
runCommentsT c = runReaderT (unpackCommentsT c)

-- | Execute the computation wrapped in a Comments monad.
runComments :: Comments a
            -> BasicHashTable Position [ByteString]
            -> IO a
runComments c = runReaderT (unpackCommentsT c)

mapCommentsT :: (Monad m, Monad n) =>
                (m a -> n b) -> CommentsT m a -> CommentsT n b
mapCommentsT f = CommentsT . mapReaderT f . unpackCommentsT

instance Monad m => Monad (CommentsT m) where
  return = CommentsT . return
  s >>= f = CommentsT $ unpackCommentsT s >>= unpackCommentsT . f

instance Monad m => Applicative (CommentsT m) where
  pure = return
  (<*>) = ap

instance (Monad m, Alternative m) => Alternative (CommentsT m) where
  empty = lift empty
  s1 <|> s2 = CommentsT (unpackCommentsT s1 <|> unpackCommentsT s2)

instance Functor (CommentsT m) where
  fmap = fmap

instance MonadIO m => MonadIO (CommentsT m) where
  liftIO = CommentsT . liftIO

instance MonadTrans CommentsT where
  lift = CommentsT . lift

instance MonadCont m => MonadCont (CommentsT m) where
  callCC f = CommentsT (callCC (\c -> unpackCommentsT (f (CommentsT . c))))

instance (Error e, MonadError e m) => MonadError e (CommentsT m) where
  throwError = lift . throwError
  m `catchError` h =
    CommentsT (unpackCommentsT m `catchError` (unpackCommentsT . h))

instance MonadGenpos m => MonadGenpos (CommentsT m) where
  position fname line = lift . position fname line

instance MonadGensym m => MonadGensym (CommentsT m) where
  symbol = lift . symbol

instance MonadKeywords t m => MonadKeywords t (CommentsT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadPositions m => MonadPositions (CommentsT m) where
  positionInfo = lift . positionInfo

instance MonadSourceFiles m => MonadSourceFiles (CommentsT m) where
  sourceFile = lift . sourceFile

instance MonadSourceLoader m => MonadSourceLoader (CommentsT m) where
  loadSourceFile = lift . loadSourceFile

instance MonadState s m => MonadState s (CommentsT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (CommentsT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadReader r m => MonadReader r (CommentsT m) where
  ask = lift ask
  local f = mapCommentsT (local f)

instance MonadWriter w m => MonadWriter w (CommentsT m) where
  tell = lift . tell
  listen = mapCommentsT listen
  pass = mapCommentsT pass

instance MonadPlus m => MonadPlus (CommentsT m) where
  mzero = lift mzero
  mplus s1 s2 = CommentsT (mplus (unpackCommentsT s1) (unpackCommentsT s2))

instance MonadFix m => MonadFix (CommentsT m) where
  mfix f = CommentsT (mfix (unpackCommentsT . f))
