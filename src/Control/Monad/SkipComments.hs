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

-- | Defines an \"implementation\" of 'MonadCommentBuffer' that skips
-- all comments rather than recording them.
module Control.Monad.SkipComments(
       MonadCommentBuffer(..),
       SkipCommentsT,
       SkipComments,
       runSkipCommentsT,
       runSkipComments
       ) where

import Control.Applicative
import Control.Monad.CommentBuffer.Class
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Genpos.Class
import Control.Monad.Gensym.Class
import Control.Monad.Keywords.Class
import Control.Monad.Loader.Class
import Control.Monad.Messages.Class
import Control.Monad.Positions.Class
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceBuffer.Class
import Control.Monad.State
import Control.Monad.Symbols.Class

-- | A monad whose \"implementation\" of 'MonadCommentBuffer' simply
-- skips all comments.
newtype SkipCommentsT m a = SkipCommentsT {
    -- | Execute the computation wrapped in a SkipCommentsT monad tranformer.
    runSkipCommentsT :: m a
  }

type SkipComments = SkipCommentsT IO

runSkipComments :: SkipComments a -> IO a
runSkipComments = runSkipCommentsT

instance Monad m => Monad (SkipCommentsT m) where
  return = SkipCommentsT . return
  s >>= f = SkipCommentsT $ runSkipCommentsT s >>= runSkipCommentsT . f

instance Monad m => Applicative (SkipCommentsT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) => Alternative (SkipCommentsT m) where
  empty = lift empty
  s1 <|> s2 =
    SkipCommentsT (runSkipCommentsT s1 <|> runSkipCommentsT s2)

instance Functor (SkipCommentsT m) where
  fmap = fmap

instance MonadIO m => MonadIO (SkipCommentsT m) where
  liftIO = SkipCommentsT . liftIO

instance MonadTrans SkipCommentsT where
  lift = SkipCommentsT

instance Monad m => MonadCommentBuffer (SkipCommentsT m) where
  startComment = SkipCommentsT (return ())
  appendComment = SkipCommentsT . const (return ())
  finishComment = SkipCommentsT (return ())
  addComment = SkipCommentsT . const (return ())
  saveCommentsAsPreceeding = SkipCommentsT . const (return ())
  clearComments = SkipCommentsT (return ())

instance MonadCont m => MonadCont (SkipCommentsT m) where
  callCC f = SkipCommentsT
    (callCC (\c -> runSkipCommentsT (f (SkipCommentsT . c))))

instance (Error e, MonadError e m) => MonadError e (SkipCommentsT m) where
  throwError = lift . throwError
  m `catchError` h =
    SkipCommentsT (runSkipCommentsT m `catchError`
                    (runSkipCommentsT . h))

instance MonadGenpos m => MonadGenpos (SkipCommentsT m) where
  position = lift . position

instance MonadGensym m => MonadGensym (SkipCommentsT m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadKeywords t m => MonadKeywords t (SkipCommentsT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadLoader path info m =>
         MonadLoader path info (SkipCommentsT m) where
  load = lift . load

instance MonadPositions m => MonadPositions (SkipCommentsT m) where
  positionInfo = lift . positionInfo

instance MonadMessages msg m => MonadMessages msg (SkipCommentsT m) where
  message = lift . message

instance MonadSourceFiles m => MonadSourceFiles (SkipCommentsT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m => MonadSourceBuffer (SkipCommentsT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (SkipCommentsT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (SkipCommentsT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadPlus m => MonadPlus (SkipCommentsT m) where
  mzero = lift mzero
  mplus s1 s2 =
    SkipCommentsT (mplus (runSkipCommentsT s1) (runSkipCommentsT s2))

instance MonadFix m => MonadFix (SkipCommentsT m) where
  mfix f = SkipCommentsT (mfix (runSkipCommentsT . f))
