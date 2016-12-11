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
-- This is the same as the "Data.Equiv" module, except that this one
-- is based on 'HashMap's.
module Data.Equivs.Hashable(
       Equivs,
       addEquiv,
       addEquivs,
       toEquivs,
       ) where

import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.HashSet(HashSet)
import Data.Monoid

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

newtype Equivs ty = Equivs { equivMap :: HashMap ty (HashSet ty) }

-- | Add an equivalence relationship to an 'Equivs'.
addEquiv :: (Eq ty, Hashable ty) =>
            Equivs ty
         -- ^ The equivalence structure to which to add the equivalence.
         -> ty
         -- ^ The first equivalent item.
         -> ty
         -- ^ The second equivalent item.
         -> Equivs ty
         -- ^ The equivalence structure with the new equivalence added.
addEquiv Equivs { equivMap = equivs } a b =
  let
    -- Make a new set with the two elements' equivalence classes unioned
    newset =  case (HashMap.lookup a equivs, HashMap.lookup b equivs) of
      (Nothing, Nothing) -> HashSet.fromList [a, b]
      (Nothing, Just set) -> HashSet.insert a set
      (Just set, Nothing) -> HashSet.insert b set
      (Just aset, Just bset) -> aset <> bset

    foldfun e = HashMap.insert e newset

    -- Update the equivalence class mapping with the new equivalence class
    newmap = HashSet.foldr foldfun equivs newset
  in
    Equivs { equivMap = newmap }

addEquivs :: (Eq ty, Hashable ty) =>
             Equivs ty
          -- ^ The equivalence structure to which to add the equivalences.
          -> [ty]
          -- ^ The equivalences to add.
          -> Equivs ty
          --  ^ The equivalence structure with the equivalences added.
addEquivs Equivs { equivMap = equivs } l =
  let
    getset a = HashMap.lookupDefault mempty a equivs

    -- Lookup and union all the equivalence sets
    newset = mconcat (map getset l)

    foldfun a = HashMap.insert a newset

    -- Update the equivalence class mapping with the new equivalence class
    newmap = HashSet.foldr foldfun equivs newset
  in
    Equivs { equivMap = newmap }

-- | Turn an 'Equivs' into a list of equivalence classes.
toEquivs :: (Eq ty, Hashable ty)=>
            Equivs ty
         -- ^ The 'Equivs' to decompose.
         -> [[ty]]
         -- ^ A list of all equivalence classes.
toEquivs Equivs { equivMap = equivs } =
  let
    pairs = HashMap.toList equivs

    foldfun (key, set) accum @ (skipset, equivsets)
      | HashSet.member key skipset = accum
      | otherwise = (skipset <> set, HashSet.toList set : equivsets)

    (_, out) = foldr foldfun (mempty, []) pairs
  in
    out

instance (Eq ty, Hashable ty) => Monoid (Equivs ty) where
  mempty = Equivs { equivMap = HashMap.empty }

  mappend Equivs { equivMap = map1 } Equivs { equivMap = map2 } =
    Equivs { equivMap = HashMap.unionWith HashSet.union map1 map2 }
