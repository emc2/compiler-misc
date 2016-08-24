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

module Control.Monad.GraphBuilder(
       MonadNodeBuilder(..),
       MonadEdgeBuilder(..),
       GraphBuilderT,
       GraphBuilder,
       runGraphBuilderT,
       runGraphBuilder
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
import Control.Monad.ScopeBuilder.Class
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceBuffer.Class
import Control.Monad.State
import Control.Monad.Symbols.Class
import Control.Monad.Writer
import Data.Graph.Inductive.Graph(Graph, Node, mkGraph)

-- | Lookup tables for converting strings to symbols and back.
type GraphData nodety edgety = ([(Node, nodety)], [(Node, Node, edgety)])

newtype GraphBuilderT nodety edgety m a =
  GraphBuilderT {
    unpackGraphBuilderT :: StateT Node (WriterT (GraphData nodety edgety) m) a
  }

type GraphBuilder nodety edgety = GraphBuilderT nodety edgety IO

-- | Execute the computation represented by a Symbols monad.
runGraphBuilder :: Graph gr =>
                   GraphBuilder nodety edgety a
                -- ^ The Symbols monad to execute.
                -> IO (a, gr nodety edgety)
runGraphBuilder = runGraphBuilderT

-- | Execute the computation wrapped in a SymbolsT monad transformer.
runGraphBuilderT :: (MonadIO m, Graph gr) =>
                    GraphBuilderT nodety edgety m a
                 -- ^ The SymbolsT monad to execute.
                 -> m (a, gr nodety edgety)
runGraphBuilderT s =
  do
    ((out, _), (nodes, edges)) <-
      runWriterT (runStateT (unpackGraphBuilderT s) 0)
    return (out, mkGraph nodes edges)

instance Monad m => Monad (GraphBuilderT nodety edgety m) where
  return = GraphBuilderT . return
  s >>= f = GraphBuilderT $ unpackGraphBuilderT s >>= unpackGraphBuilderT . f

instance Monad m => Applicative (GraphBuilderT nodety edgety m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) =>
         Alternative (GraphBuilderT nodety edgety m) where
  empty = lift empty
  s1 <|> s2 = GraphBuilderT (unpackGraphBuilderT s1 <|> unpackGraphBuilderT s2)

instance Functor (GraphBuilderT nodety edgety m) where
  fmap = fmap

instance MonadIO m =>
         MonadNodeBuilder nodety (GraphBuilderT nodety edgety m) where
  addNode ndata =
    let
      action =
        do
          idx <- get
          put $! idx + 1
          tell ([(idx, ndata)], [])
          return idx
    in
      GraphBuilderT action

instance MonadIO m =>
         MonadEdgeBuilder edgety (GraphBuilderT nodety edgety m) where
  addEdge srcnode dstnode edgedata =
    GraphBuilderT (tell ([], [(srcnode, dstnode, edgedata)]))

instance MonadArtifacts path m =>
         MonadArtifacts path (GraphBuilderT nodety edgety m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadCommentBuffer m =>
         MonadCommentBuffer (GraphBuilderT nodety edgety m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadGenpos m =>
         MonadGenpos (GraphBuilderT nodety edgety m) where
  point = lift . point
  filename = lift . filename

instance MonadGensym m =>
         MonadGensym (GraphBuilderT nodety edgety m) where
  symbol = GraphBuilderT . symbol
  unique = GraphBuilderT . unique

instance MonadIO m =>
         MonadIO (GraphBuilderT nodety edgety m) where
  liftIO = GraphBuilderT . liftIO

instance MonadTrans (GraphBuilderT nodety edgety) where
  lift = GraphBuilderT . lift . lift

instance MonadComments m =>
         MonadComments (GraphBuilderT nodety edgety m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m =>
         MonadCont (GraphBuilderT nodety edgety m) where
  callCC f = GraphBuilderT (callCC (\c -> unpackGraphBuilderT (f (GraphBuilderT . c))))

instance (MonadError e m) =>
         MonadError e (GraphBuilderT nodety edgety m) where
  throwError = lift . throwError
  m `catchError` h =
    GraphBuilderT (unpackGraphBuilderT m `catchError` (unpackGraphBuilderT . h))

instance (Monoid w, MonadJournal w m) =>
         MonadJournal w (GraphBuilderT nodety edgety m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m =>
         MonadKeywords p t (GraphBuilderT nodety edgety m) where
  mkKeyword p = lift . mkKeyword p

instance MonadLoader path info m =>
         MonadLoader path info (GraphBuilderT nodety edgety m) where
  load = lift . load

instance MonadMessages msg m =>
         MonadMessages msg (GraphBuilderT nodety edgety m) where
  message = lift . message

instance MonadPositions m =>
         MonadPositions (GraphBuilderT nodety edgety m) where
  pointInfo = lift . pointInfo
  fileInfo = lift . fileInfo

instance MonadScopeStack m =>
         MonadScopeStack (GraphBuilderT nodety edgety m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance MonadScopeBuilder tmpscope m =>
         MonadScopeBuilder tmpscope (GraphBuilderT nodety edgety m) where
  alterScope = lift . alterScope

instance MonadSourceFiles m =>
         MonadSourceFiles (GraphBuilderT nodety edgety m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m =>
         MonadSourceBuffer (GraphBuilderT nodety edgety m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m =>
         MonadState s (GraphBuilderT nodety edgety m) where
  get = lift get
  put = lift . put

instance MonadSymbols m =>
         MonadSymbols (GraphBuilderT nodety edgety m) where
  nullSym = GraphBuilderT nullSym
  allNames = GraphBuilderT allNames
  allSyms = GraphBuilderT allSyms
  name = GraphBuilderT . name

instance MonadPlus m =>
         MonadPlus (GraphBuilderT nodety edgety m) where
  mzero = lift mzero
  mplus s1 s2 = GraphBuilderT (mplus (unpackGraphBuilderT s1)
                                     (unpackGraphBuilderT s2))

instance MonadFix m =>
         MonadFix (GraphBuilderT nodety edgety m) where
  mfix f = GraphBuilderT (mfix (unpackGraphBuilderT . f))
