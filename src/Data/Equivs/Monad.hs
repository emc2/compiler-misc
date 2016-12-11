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

-- | A data structure for equivalence classes.  This provides an
-- interface whereby equivalence relations can be added to a
-- collection, then all the equivalence classes implied by the
-- relationships that have been added can be extracted.
--
-- This is similar to the "Data.Equiv" module, except it operates
-- inside 'MonadIO' and uses 'BasicHashTable's
module Data.Equivs.Monad(
       Equivs,
       new,
       addEquiv,
       addEquivs,
       toEquivs,
       ) where

import Control.Monad.Trans
import Data.Hashable
import Data.HashSet(HashSet)
import Data.HashTable.IO(BasicHashTable)
import Data.Monoid

import qualified Data.HashTable.IO as HashTable
import qualified Data.HashSet as HashSet

newtype Equivs ty = Equivs { equivMap :: BasicHashTable ty (HashSet ty) }

-- | Create an empty 'Equivs'.
new :: MonadIO m =>
       m (Equivs ty)
new =
  do
    tab <- liftIO HashTable.new
    return Equivs { equivMap = tab }

-- | Add an equivalence relationship to an 'Equivs'.
addEquiv :: (Eq ty, Hashable ty, MonadIO m) =>
            Equivs ty
         -- ^ The equivalence structure to which to add the equivalence.
         -> ty
         -- ^ The first equivalent item.
         -> ty
         -- ^ The second equivalent item.
         -> m ()
addEquiv Equivs { equivMap = equivs } a b =
  let
    -- Make a new set with the two elements' equivalence classes unioned
    newset =
      do
        aent <- HashTable.lookup equivs a
        bent <- HashTable.lookup equivs b
        case (aent, bent) of
          (Nothing, Nothing) -> return (HashSet.fromList [a, b])
          (Nothing, Just set) -> return (HashSet.insert a set)
          (Just set, Nothing) -> return (HashSet.insert b set)
          (Just aset, Just bset) -> return (aset <> bset)

    mapfun set e = liftIO (HashTable.insert equivs e set)
  in do
    set <- liftIO newset
    -- Update the equivalence class mapping with the new equivalence class
    mapM_ (mapfun set) (HashSet.toList set)

addEquivs :: (Eq ty, Hashable ty, MonadIO m) =>
             Equivs ty
          -- ^ The equivalence structure to which to add the equivalences.
          -> [ty]
          -- ^ The equivalences to add.
          -> m ()
addEquivs Equivs { equivMap = equivs } l =
  let
    getset a =
      do
        res <- HashTable.lookup equivs a
        case res of
          Just out -> return out
          Nothing -> return mempty

    -- Lookup and union all the equivalence sets
    newset =
      do
        sets <- mapM getset l
        return (mconcat sets)

    mapfun set a = liftIO (HashTable.insert equivs a set)
  in do
    set <- liftIO newset
    mapM_ (mapfun set) (HashSet.toList set)

-- | Turn an 'Equivs' into a list of equivalence classes.
toEquivs :: (Eq ty, Hashable ty, MonadIO m) =>
            Equivs ty
         -- ^ The 'Equivs' to decompose.
         -> m [[ty]]
         -- ^ A list of all equivalence classes.
toEquivs Equivs { equivMap = equivs } =
  let
    foldfun accum @ (skipset, equivsets) (key, set)
      | HashSet.member key skipset = accum
      | otherwise = (skipset <> set, HashSet.toList set : equivsets)

    getsets = snd . foldl foldfun (mempty, [])
  in do
    pairs <- liftIO (HashTable.toList equivs)
    return (getsets pairs)