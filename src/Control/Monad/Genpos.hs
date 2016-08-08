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

module Control.Monad.Genpos(
       MonadGenpos(..),
       MonadPositions(..),
       GenposT,
       Genpos,
       runGenposT,
       runGenpos,
       startGenposT,
       startGenpos,
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
import Control.Monad.Reader
import Control.Monad.SourceFiles.Class
import Control.Monad.SourceBuffer.Class
import Control.Monad.State
import Control.Monad.Symbols.Class
import Data.HashTable.IO(BasicHashTable)
import Data.Position.Filename
import Data.Position.Point
import Data.Maybe

import qualified Data.HashTable.IO as HashTable

data Bounds = Bounds { loPos :: !Point, hiPos :: !Point,
                       loFile :: !Filename, hiFile :: !Filename }

data Table = Table { posTable :: !(BasicHashTable Point PointInfo),
                     posRevTable :: !(BasicHashTable PointInfo Point),
                     fileTable :: !(BasicHashTable Filename FileInfo),
                     fileRevTable :: !(BasicHashTable FileInfo Filename)}

newtype GenposT m a =
  GenposT { unpackGenposT :: StateT Bounds (ReaderT Table m) a }

type Genpos = GenposT IO

-- | Execute the computation represented by a Genpos monad.
runGenpos :: Genpos a
          -- ^ The Symbols monad to execute.
          -> (Point, Point)
          -- ^ The low and high range of the positions.
          -> [(Point, PointInfo)]
          -- ^ The mapping of symbols.  The mapping to the lowest
          -- index is taken as the null symbol.
          -> (Filename, Filename)
          -- ^ The low and high range of the symbols.
          -> [(Filename, FileInfo)]
          -- ^ The mapping of symbols.  The mapping to the lowest
          -- index is taken as the null symbol.
          -> IO a
runGenpos = runGenposT

-- | Execute the computation wrapped in a GenposT monad transformer.
runGenposT :: MonadIO m =>
              GenposT m a
           -- ^ The SymbolsT monad to execute.
           -> (Point, Point)
           -- ^ The low and high range of the positions.  The lowest
           -- index is used as the index of the null symbol.
           -> [(Point, PointInfo)]
           -- ^ The mapping of symbols to indexes.  The mapping to the
           -- lowest index is taken as the null symbol.
           -> (Filename, Filename)
           -- ^ The low and high range of the symbols.
           -> [(Filename, FileInfo)]
           -- ^ The mapping of symbols.  The mapping to the lowest
           -- index is taken as the null symbol.
           -> m a
runGenposT s (lopos, hipos) positions (lofile, hifile) files =
  let
    revpositions = map (\(a, b) -> (b, a)) positions
    revfiles = map (\(a, b) -> (b, a)) files
  in do
    postab <- liftIO (HashTable.fromList positions)
    revpostab <- liftIO (HashTable.fromList revpositions)
    filetab <- liftIO (HashTable.fromList files)
    revfiletab <- liftIO (HashTable.fromList revfiles)
    (out, _) <- runReaderT (runStateT (unpackGenposT s)
                                      Bounds { loPos = lopos,
                                               hiPos = hipos,
                                               loFile = lofile,
                                               hiFile = hifile})
                           Table { posTable = postab,
                                   posRevTable = revpostab,
                                   fileTable = filetab,
                                   fileRevTable = revfiletab }
    return out

-- | Execute a Genpos monad with a starting state with only the null
-- symbol defined
startGenpos :: Genpos a
            -- ^ The Symbols monad to execute.
            -> IO a
startGenpos = startGenposT

-- | Execute a GenposT monad transformer with a starting state with
-- only the null symbol defined
startGenposT :: MonadIO m =>
                GenposT m a
             -- ^ The SymbolsT monad to execute.
             -> m a
startGenposT s =
  runGenposT s (firstPoint, firstPoint) [] (firstFilename, firstFilename) []

point' :: MonadIO m =>
          PointInfo
       -> (StateT Bounds (ReaderT Table m)) Point
point' pos =
  do
    Table { posTable = tab, posRevTable = revtab } <- ask
    res <- liftIO $! HashTable.lookup revtab pos
    case res of
      Just out -> return out
      Nothing ->
        do
          bounds @ Bounds { hiPos = hi } <- get
          put bounds { hiPos = succ hi }
          liftIO (HashTable.insert tab hi pos)
          return hi

filename' :: MonadIO m =>
             FileInfo
          -> (StateT Bounds (ReaderT Table m)) Filename
filename' fileinfo =
  do
    Table { fileTable = tab, fileRevTable = revtab } <- ask
    res <- liftIO $! HashTable.lookup revtab fileinfo
    case res of
      Just out -> return out
      Nothing ->
        do
          bounds @ Bounds { hiFile = hi } <- get
          put bounds { hiFile = succ hi }
          liftIO (HashTable.insert tab hi fileinfo)
          return hi

pointInfo' :: MonadIO m => Point -> (StateT Bounds (ReaderT Table m)) PointInfo
pointInfo' pos =
  do
    Table { posTable = tab } <- ask
    res <- liftIO (HashTable.lookup tab pos)
    return (fromJust res)

fileInfo' :: MonadIO m => Filename ->
             (StateT Bounds (ReaderT Table m)) FileInfo
fileInfo' file =
  do
    Table { fileTable = tab } <- ask
    res <- liftIO (HashTable.lookup tab file)
    return (fromJust res)

instance Monad m => Monad (GenposT m) where
  return = GenposT . return
  s >>= f = GenposT $ unpackGenposT s >>= unpackGenposT . f

instance Monad m => Applicative (GenposT m) where
  pure = return
  (<*>) = ap

instance (MonadPlus m, Alternative m) => Alternative (GenposT m) where
  empty = lift empty
  s1 <|> s2 = GenposT (unpackGenposT s1 <|> unpackGenposT s2)

instance Functor (GenposT m) where
  fmap = fmap

instance MonadIO m => MonadGenpos (GenposT m) where
  point = GenposT . point'
  filename = GenposT . filename'

instance MonadCommentBuffer m => MonadCommentBuffer (GenposT m) where
  startComment = lift startComment
  appendComment = lift . appendComment
  finishComment = lift finishComment
  addComment = lift . addComment
  saveCommentsAsPreceeding = lift . saveCommentsAsPreceeding
  clearComments = lift clearComments

instance MonadIO m => MonadPositions (GenposT m) where
  pointInfo = GenposT . pointInfo'
  fileInfo = GenposT . fileInfo'

instance MonadIO m => MonadIO (GenposT m) where
  liftIO = GenposT . liftIO

instance MonadTrans GenposT where
  lift = GenposT . lift . lift

instance MonadArtifacts path m => MonadArtifacts path (GenposT m) where
  artifact path = lift . artifact path
  artifactBytestring path = lift . artifactBytestring path
  artifactLazyBytestring path = lift . artifactLazyBytestring path

instance MonadComments m => MonadComments (GenposT m) where
  preceedingComments = lift . preceedingComments

instance MonadCont m => MonadCont (GenposT m) where
  callCC f = GenposT (callCC (\c -> unpackGenposT (f (GenposT . c))))

instance MonadEdgeBuilder nodety m =>
         MonadEdgeBuilder nodety (GenposT m) where
  addEdge src dst = lift . addEdge src dst

instance (MonadError e m) => MonadError e (GenposT m) where
  throwError = lift . throwError
  m `catchError` h =
    GenposT (unpackGenposT m `catchError` (unpackGenposT . h))

instance MonadGensym m => MonadGensym (GenposT m) where
  symbol = lift . symbol
  unique = lift . unique

instance (Monoid w, MonadJournal w m) => MonadJournal w (GenposT m) where
  journal = lift . journal
  history = lift history
  clear = lift clear

instance MonadKeywords p t m => MonadKeywords p t (GenposT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadLoader path info m => MonadLoader path info (GenposT m) where
  load = lift . load

instance MonadMessages msg m => MonadMessages msg (GenposT m) where
  message = lift . message

instance MonadNodeBuilder nodety m =>
         MonadNodeBuilder nodety (GenposT m) where
  addNode = lift . addNode

instance MonadSourceFiles m => MonadSourceFiles (GenposT m) where
  sourceFile = lift . sourceFile

instance MonadSourceBuffer m => MonadSourceBuffer (GenposT m) where
  linebreak = lift . linebreak
  startFile fname = lift . startFile fname
  finishFile = lift finishFile

instance MonadState s m => MonadState s (GenposT m) where
  get = lift get
  put = lift . put

instance MonadSymbols m => MonadSymbols (GenposT m) where
  nullSym = lift nullSym
  allNames = lift allNames
  allSyms = lift allSyms
  name = lift . name

instance MonadPlus m => MonadPlus (GenposT m) where
  mzero = lift mzero
  mplus s1 s2 = GenposT (mplus (unpackGenposT s1) (unpackGenposT s2))

instance MonadFix m => MonadFix (GenposT m) where
  mfix f = GenposT (mfix (unpackGenposT . f))
