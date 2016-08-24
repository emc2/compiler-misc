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
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Control.Monad.ScopeBuilder(
       module Control.Monad.ScopeBuilder.Class,
       ScopeBuilderT,
       ScopeBuilder,
       runScopeBuilderT,
       runScopeBuilder
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
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceBuffer.Class
import Control.Monad.ScopeBuilder.Class
import Control.Monad.State
import Control.Monad.Symbols.Class
import Data.Array
import Data.Position.BasicPosition
import Data.ScopeID

-- | State of the scope builder.
data ScopeBuilderState tmpscope scope =
  ScopeBuilderState {
    -- | Running counter of scopeIDs
    builderNextScope :: !ScopeID,
    -- | A stack of scopes.  We add definitions into the top of the stack.
    builderScopes :: ![tmpscope],
    -- | Scopes that have been finished.  This will be turned into an
    -- array at the end of collect.
    builderFinishedScopes :: ![(ScopeID, scope)]
  }

newtype ScopeBuilderT tmpscope scope m a =
  ScopeBuilderT {
    unpackScopeBuilderT :: StateT (ScopeBuilderState tmpscope scope) m a
  }

type ScopeBuilder tmpscope scope = ScopeBuilderT tmpscope scope IO

emptyState :: ScopeBuilderState tmpscope scope
emptyState =  ScopeBuilderState { builderNextScope = toEnum 0,
                                  builderScopes = [],
                                  builderFinishedScopes = [] }

runScopeBuilderT :: Monad m =>
                    ScopeBuilderT tmpscope scope m a
                 -- ^ ScopeBuilderT monad transformer to run.
                 -> m (a, Array ScopeID scope)
runScopeBuilderT ScopeBuilderT { unpackScopeBuilderT = s } =
  do
    (res, ScopeBuilderState { builderNextScope = lastid, builderScopes = scopes,
                              builderFinishedScopes = finscopes }) <-
      runStateT s emptyState
    case scopes of
      [] ->
        return (res, array (firstScopeID, pred lastid) finscopes)
      _ -> error "Leftover scope frames at end of scope building"

runScopeBuilder :: ScopeBuilder tmpscope scope a
                -- ^ ScopeBuilderT monad transformer to run.
                -> IO (a, Array ScopeID scope)
runScopeBuilder = runScopeBuilderT

enterScope' :: (Monad m, TempScope tmpscope scope m) =>
               BasicPosition -> StateT (ScopeBuilderState tmpscope scope) m ()
enterScope' pos =
  do
    currstate @ ScopeBuilderState { builderScopes = scopes } <- get
    newscope <- lift $! createSubscope pos scopes
    put currstate { builderScopes = newscope : scopes }

finishScope' :: (Monad m, TempScope tmpscope scope m) =>
               StateT (ScopeBuilderState tmpscope scope) m ScopeID
finishScope' =
  do
    currstate @ ScopeBuilderState { builderFinishedScopes = finscopes,
                                    builderNextScope = scopeid,
                                    builderScopes = scopes } <- get
    case scopes of
      [] -> error "Scope stack underflow!"
      first : rest ->
        do
          finscope <- lift $! finalizeScope first
          put currstate { builderFinishedScopes = (scopeid, finscope) :
                                                  finscopes,
                         builderNextScope = succ scopeid,
                         builderScopes = rest }
          return scopeid

alterScope' :: (Monad m, TempScope tmpscope scope m) =>
               (tmpscope -> tmpscope)
            -> StateT (ScopeBuilderState tmpscope scope) m ()
alterScope' func =
  do
    currstate @ ScopeBuilderState { builderScopes = scopes } <- get
    case scopes of
      [] -> error "Scope stack underflow!"
      first : rest -> put currstate { builderScopes = func first : rest }

instance Monad m => Monad (ScopeBuilderT tmpscope scope m) where
  return = ScopeBuilderT . return
  s >>= f = ScopeBuilderT $ unpackScopeBuilderT s >>= unpackScopeBuilderT . f

instance Monad m => Applicative (ScopeBuilderT tmpscope scope m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) =>
         Alternative (ScopeBuilderT tmpscope scope m) where
  empty = lift empty
  s1 <|> s2 = ScopeBuilderT (unpackScopeBuilderT s1 <|> unpackScopeBuilderT s2)

instance Functor (ScopeBuilderT tmpscope scope m) where
  fmap = fmap

instance MonadArtifacts path m =>
         MonadArtifacts path (ScopeBuilderT tmpscope scope m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCommentBuffer m =>
         MonadCommentBuffer (ScopeBuilderT tmpscope scope m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadGenpos m =>
         MonadGenpos (ScopeBuilderT tmpscope scope m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m => MonadGensym (ScopeBuilderT tmpscope scope m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadIO m => MonadIO (ScopeBuilderT tmpscope scope m) where
  liftIO = ScopeBuilderT . liftIO

instance MonadTrans (ScopeBuilderT tmpscope scope) where
  lift = ScopeBuilderT . lift

instance MonadComments m => MonadComments (ScopeBuilderT tmpscope scope m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (ScopeBuilderT tmpscope scope m) where
  callCC f =
    ScopeBuilderT (callCC (\c -> unpackScopeBuilderT (f (ScopeBuilderT . c))))

instance (MonadError e m) => MonadError e (ScopeBuilderT tmpscope scope m) where
  throwError = lift . throwError
  m `catchError` h =
    ScopeBuilderT (unpackScopeBuilderT m `catchError` (unpackScopeBuilderT . h))

instance MonadEdgeBuilder nodety m =>
         MonadEdgeBuilder nodety (ScopeBuilderT tmpscope scope m) where
  addEdge src dst = lift . addEdge src dst

instance (Monoid w, MonadJournal w m) =>
         MonadJournal w (ScopeBuilderT tmpscope scope m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m =>
         MonadKeywords p t (ScopeBuilderT tmpscope scope m) where
  mkKeyword p = lift . mkKeyword p

instance MonadLoader path info m =>
         MonadLoader path info (ScopeBuilderT tmpscope scope m) where
  load = lift . load

instance MonadMessages msg m =>
         MonadMessages msg (ScopeBuilderT tmpscope scope m) where
  message = lift . message

instance MonadNodeBuilder nodety m =>
         MonadNodeBuilder nodety (ScopeBuilderT tmpscope scope m) where
  addNode = lift . addNode

instance MonadPositions m =>
         MonadPositions (ScopeBuilderT tmpscope scope m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance (Monad m, TempScope tmpscope scope m) =>
         MonadScopeStack (ScopeBuilderT tmpscope scope m) where
  enterScope = ScopeBuilderT . enterScope'
  finishScope = ScopeBuilderT finishScope'

instance (Monad m, TempScope tmpscope scope m) =>
         MonadScopeBuilder tmpscope (ScopeBuilderT tmpscope scope m) where
  alterScope = ScopeBuilderT . alterScope'

instance MonadSourceFiles m =>
         MonadSourceFiles (ScopeBuilderT tmpscope scope m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m =>
         MonadSourceBuffer (ScopeBuilderT tmpscope scope m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadSymbols m => MonadSymbols (ScopeBuilderT tmpscope scope m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadState s m => MonadState s (ScopeBuilderT tmpscope scope m) where
  get = lift get
  put = lift . put

instance MonadPlus m => MonadPlus (ScopeBuilderT tmpscope scope m) where
  mzero = lift mzero
  mplus s1 s2 = ScopeBuilderT (mplus (unpackScopeBuilderT s1)
                                     (unpackScopeBuilderT s2))

instance MonadFix m => MonadFix (ScopeBuilderT tmpscope scope m) where
  mfix f = ScopeBuilderT (mfix (unpackScopeBuilderT . f))
