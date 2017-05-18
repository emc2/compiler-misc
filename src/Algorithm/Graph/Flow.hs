-- Copyright (c) 2016 Eric McCorkle.  All rights reserved.
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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}

-- | A generalized implementation of flow analysis.  This algorithm
-- operates on a graph, propagating information from each node to its
-- outgoing neighbors until the graph reaches a stable fixed point.
-- This algorithm is extremely common in compilers and program
-- analysis, and many problems can be characterized and solved using
-- it.
--
-- In order to use this implementation, the node data needs to have a
-- 'Monoid' instance, and a 'FlowData' instance needs to be
-- provided for the node and edge data.  The 'Monoid' instance should
-- provide a way to merge information about nodes, producing a new
-- node with the combined information of the two arguments.  The
-- 'propagate' function in the 'FlowData' instance should
-- propagate information from a node along a given edge, producing a
-- new node type containing the information that was propagated.  This
-- will then be merged with the neighbor using the 'mappend' function.
module Algorithm.Graph.Flow(
       FlowData(..),
       flow,
       ) where

import Algorithm.Worklist
import Control.Monad.Trans
import Data.Graph.Inductive.Graph
import Data.Maybe

-- | An abstract characterization of the data for a flow analysis
-- problem.  In flow analysis, we propagate all nodes to all
-- neighbors, then combine all the incoming edges for each node
-- according to another operation.
--
-- We assume here that the 'Monoid' operation on node data represents
-- the merge.  The 'propagate' operation then propagates each node's
-- incoming edges, then merges the results with the existing node
-- data.
--
-- For the flow analysis to terminate, it must be the case that all
-- functions formed out of 'mappend' and 'propagate' converge to a
-- fixed point with repeated application.  If this property fails,
-- then the flow alorithm may not terminate.
class Monoid nodety => FlowData nodety edgety where
  -- | Compute the effects of propagating node data over a given edge.
  -- This should produce a node data element that will be combined
  -- with all the others using the monoid instance.
  propagate :: Monad m =>
               nodety
            -- ^ The source node data.
            -> edgety
            -- ^ The edge over which to propagate.
            -> m nodety
            -- ^ The result of propagating the source data over the
            -- edge, or 'Nothing' if the node doesn't change.

-- | Perform flow analysis on the graph.  This will propagate each
-- node along its outgoing edges as per the 'propagate' function,
-- merging with each node by the 'mappend' function from the 'Monoid'
-- instance.  The process will continue until the graph reaches a
-- stable fixed point.
flow :: forall nodety edgety gr m.
        (FlowData nodety edgety, DynGraph gr, MonadIO m,
         Monoid nodety, Eq nodety) =>
        gr nodety edgety
     -- ^ The initial graph state.
     -> [Node]
     -- ^ The starting nodes.
     -> m (gr nodety edgety)
     -- ^ The graph state after doing flow analysis
flow initgraph =
  let
    -- Work function for a given node.  Propagate all inbound
    -- neighbors over the inbound edges, merge them together with the
    -- current node state using the monoid operation.
    workfunc :: gr nodety edgety -> Int -> m (Maybe (gr nodety edgety))
    workfunc graph idx =
      let
        node :: nodety
        node = fromMaybe mempty (lab graph idx)

        mapfun (from, _, edgedata) =
          case lab graph from of
            Just fromnode -> propagate fromnode edgedata
            Nothing -> return mempty

        inedges = inn graph idx
      in do
        mergelist <- mapM mapfun inedges
        newnode <- return $! mconcat (node : mergelist)
        if newnode == node
          then return Nothing
          else return (Just (insNode (idx, newnode) graph))
  in
    worklistFastIO workfunc initgraph (nodeRange initgraph)
