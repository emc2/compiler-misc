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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts #-}

-- | This module provides a generalized implementation of a worklist
-- algorithm: a very useful algorithm commonly used in compiler
-- implementation.
--
-- The worklist algorith scheme consists of three components:, a state
-- consisting of various independent components indexed by some index
-- type, the ability to determine which components depend on which
-- other components (i.e. the antidependendencies for a given index),
-- and a work function that recalculates the component for a given
-- index.  We start at a given set of indexes and recalculate each
-- one.  if the recalculation changes the state at that index, then we
-- recalculate its antidependencies as well.  The algorithm proceeds
-- until recalculation of any index would not change the state.
--
-- This is implemented by keeping a list of indexes to recalculate
-- (hence the name of the algorithm).  At each step, we remove an
-- index from the worklist and recalculate it.  If the recalculation
-- of an index produces a change, then its antidependencies are added
-- to the worklist.  The procedure is repeated until the worklist is
-- empty.
--
-- To use this module, you'll need to provide an instance of
-- 'WorklistState' or 'WorklistStateM' for the overall state.  The
-- state is expected to be indexed, and each index refers to an
-- independent section of state.  A 'WorklistState' (or
-- 'WorklistStateM' instance) provides a way to get all indexes which
-- depend on a given index.  A graph, with indexes referring to nodes
-- is an appropriate model for this, and an instance is indeed
-- provided for any datatypes implementing the 'Graph' typeclass.
--
-- Three worklist implementations are provided.  A purely-functional
-- list-based implementation is provided in 'worklist', and requires a
-- 'WorklistState' instance.  A monadic implementation is provided by
-- 'worklistM', which requires a 'WorklistStateM' instance.  The
-- 'worklistFastIO' implementation uses bitsets in lieu of a list, and
-- therefore avoids a significant amount of allocation.  This requires
-- a 'WorklistStateM' instance, and requires that the index type be
-- 'Int'.  It is recommended to use 'worklistFastIO' wherever possible.
module Algorithm.Worklist(
       WorklistState(..),
       WorklistStateM(..),
       worklist,
       worklistM,
       worklistFastIO
       ) where

import Control.Monad.Trans
import Data.Array.BitArray.IO(IOBitArray)
import Data.Graph.Inductive.Graph

import qualified Data.Array.BitArray.IO as BitArray.IO

-- | A typeclass defining methods needed for the worklist state and
-- elements.  The whole state is represented by 'statety', and an
-- index into the state is represented by 'idxty'.
class WorklistState idxty statety where
  -- | Get all elements that depend on a given element.
  antideps :: statety
           -- ^ The complete state.
           -> idxty
           -- ^ The element whose antidependencies to get.
           -> [idxty]
           -- ^ All elements that depend on the specified parameter.

-- | A typeclass defining methods needed for the worklist state and
-- elements, when those methods operate inside a monad.  Otherwise,
-- this is identical to 'WorklistState'.
class Monad m => WorklistStateM idxty statety m where
  -- | Get all elements that depend on a given element.
  antidepsM :: statety
            -- ^ The complete state.
            -> idxty
            -- ^ The element whose antidependencies to get.
            -> m [idxty]
            -- ^ All elements that depend on the specified parameter.

instance (Monad m, WorklistState idxty statety) =>
         WorklistStateM idxty statety m where
  antidepsM state = return . antideps state

instance Graph gr => WorklistState Int (gr nodety edgety) where
  antideps = pre

-- | Run a worklist algorithm.  Given a work function that transforms
-- an element of the state and an initial worklist, apply the work
-- function to each element in the worklist.  If the work function
-- makes changes, then all antidependencies (elements that depend on
-- the given element) will be added to the worklist.  The algorithm
-- proceeds until the worklist is exhausted.
worklist :: forall idxty statety.
            WorklistState idxty statety =>
            (statety -> idxty -> Maybe statety)
         -- ^ The work function.  Run on an element of the state,
         -- return @Just@ new state if something changed, or
         -- @Nothing@ if the state is unchanged.
         -> statety
         -- ^ The initial state.
         -> [idxty]
         -- ^ The initial worklist.
         -> statety
         -- ^ The state after running the worklist algorithm.
worklist workfunc =
  let
    step :: statety -> [idxty] -> statety
    step state [] = state
    step state (first : rest) =
      case workfunc state first of
        Nothing -> step state rest
        Just newstate -> step newstate (rest ++ antideps newstate first)
  in
    step

-- | A monadic variant of the worklist algorithm.  This is similar in
-- function to 'runWorklist', but allows the work function to be
-- monadic.
worklistM :: forall idxty statety m.
             WorklistStateM idxty statety m =>
             (statety -> idxty -> m (Maybe statety))
          -- ^ The work function.  Run on an element of the state,
          -- return @Just@ new state if something changed, or
          -- @Nothing@ if the state is unchanged.
          -> statety
          -- ^ The initial state.
          -> [idxty]
          -- ^ The initial worklist.
          -> m statety
          -- ^ The state after running the worklist algorithm.
worklistM workfunc =
  let
    step :: statety -> [idxty] -> m statety
    step state [] = return state
    step state (first : rest) =
      do
        res <- workfunc state first
        case res of
          Nothing -> step state rest
          Just newstate ->
            do
              newelems <- antidepsM newstate first
              step newstate (rest ++ newelems)
  in
    step

-- | A worklist implementation that uses bit-sets instead of lists to
-- store the worklist.
worklistFastIO :: forall statety m.
                  (MonadIO m, WorklistStateM Int statety m) =>
                  (statety -> Int -> m (Maybe statety))
               -- ^ The work function.  Run on an element of the state,
               -- return @Just@ new state if something changed, or
               -- @Nothing@ if the state is unchanged.
               -> statety
               -- ^ The initial state.
               -> (Int, Int)
               -- ^ The bounds on element indexes
               -> [Int]
               -- ^ The initial worklist.
               -> m statety
               -- ^ The state after running the worklist algorithm.
worklistFastIO workfunc initstate bounds initelems =
  let
    setbits :: IOBitArray Int -> [Int] -> IO ()
    setbits bitarr = mapM_ (\idx -> BitArray.IO.writeArray bitarr idx True)

    step :: IOBitArray Int -> statety -> m statety
    step bitarr state =
      do
        res <- liftIO $! BitArray.IO.elemIndex True bitarr
        case res of
          Just idx ->
            do
              liftIO $! BitArray.IO.writeArray bitarr idx False
              res' <- workfunc state idx
              case res' of
                Just newstate ->
                  do
                    newelems <- antidepsM newstate idx
                    liftIO $! setbits bitarr newelems
                    step bitarr newstate
                Nothing -> step bitarr state

          Nothing -> return state
  in do
    arr <- liftIO $! BitArray.IO.newArray bounds False
    liftIO $! setbits arr initelems
    step arr initstate
