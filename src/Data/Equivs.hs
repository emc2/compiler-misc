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
module Data.Equivs(
       Equivs,
       addEquiv,
       addEquivs,
       toEquivs,
       ) where

import Data.Map.Strict(Map)
import Data.Set(Set)
import Data.Monoid

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | A set of equivalence relations.
newtype Equivs idty infoty = Equivs { equivMap :: Map idty (Set idty, infoty) }

-- | Add an equivalence relationship to an 'Equivs'.
addEquiv :: (Ord idty, Monoid infoty) =>
            idty
         -- ^ The first equivalent item.
         -> idty
         -- ^ The second equivalent item.
         -> infoty
         -- ^ The extra information.
         -> Equivs idty infoty
         -- ^ The equivalence structure to which to add the equivalence.
         -> Equivs idty infoty
         -- ^ The equivalence structure with the new equivalence added.
addEquiv a b info Equivs { equivMap = equivs } =
  let
    -- Make a new set with the two elements' equivalence classes unioned
    newent @ (set, _) = case (Map.lookup a equivs, Map.lookup b equivs) of
      (Nothing, Nothing) -> (Set.fromList [a, b], info)
      (Nothing, Just (oldset, oldinfo)) ->
        (Set.insert a oldset, info <> oldinfo)
      (Just (oldset, oldinfo), Nothing) ->
        (Set.insert b oldset, info <> oldinfo)
      (Just (aset, ainfo), Just (bset, binfo)) ->
        (aset <> bset, info <> ainfo <> binfo)

    foldfun e = Map.insert e newent

    -- Update the equivalence class mapping with the new equivalence class
    newmap = Set.foldr foldfun equivs set
  in
    Equivs { equivMap = newmap }

-- | Add a set of equivalences to an 'Equivs'.
addEquivs :: (Ord idty, Monoid infoty) =>
             [idty]
          -- ^ The equivalences to add.
          -> infoty
          -- ^ The extra information.
          -> Equivs idty infoty
          -- ^ The equivalence structure to which to add the equivalences.
          -> Equivs idty infoty
          --  ^ The equivalence structure with the equivalences added.
addEquivs l info Equivs { equivMap = equivs } =
  let
    getset a = Map.findWithDefault (mempty, mempty) a equivs

    -- Lookup and union all the equivalence sets
    (set, newinfo) = mconcat (map getset l)

    newent = (set, info <> newinfo)

    foldfun a = Map.insert a newent

    -- Update the equivalence class mapping with the new equivalence class
    newmap = Set.foldr foldfun equivs set
  in
    Equivs { equivMap = newmap }

-- | Turn an 'Equivs' into a list of equivalence classes.
toEquivs :: (Ord idty) =>
            Equivs idty infoty
         -- ^ The 'Equivs' to decompose.
         -> [([idty], infoty)]
         -- ^ A list of all equivalence classes.
toEquivs Equivs { equivMap = equivs } =
  let
    pairs = Map.toList equivs

    foldfun (key, (set, info)) accum @ (skipset, equivsets)
      | Set.member key skipset = accum
      | otherwise = (skipset <> set, (Set.toList set, info) : equivsets)

    (_, out) = foldr foldfun (mempty, []) pairs
  in
    out

instance (Ord idty, Monoid infoty) =>
         Monoid (Equivs idty infoty) where
  mempty = Equivs { equivMap = Map.empty }

  mappend Equivs { equivMap = map1 } Equivs { equivMap = map2 } =
    Equivs { equivMap = Map.unionWith mappend map1 map2 }
