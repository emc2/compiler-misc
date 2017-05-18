-- Copyright (c) 2013 Eric McCorkle.  All rights reserved.
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
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- | This module contains extra instances of Hashable, which are not
-- found in the data-hash library, but are useful.
module Data.Hashable.ExtraInstances where

import Data.Array(Array)
import Data.Hashable
--import Data.Hashable.Extras
import Data.Map(Map)

import qualified Data.Array as Array
import qualified Data.Map as Map

--instance Hashable2 Map
--instance Hashable k => Hashable1 (Map k)

instance (Hashable k, Hashable v) => Hashable (Map k v) where
  hashWithSalt =
    Map.foldWithKey (\k' v' s' -> s' `hashWithSalt` k' `hashWithSalt` v')

instance (Hashable i, Hashable e, Array.Ix i) => Hashable (Array i e) where
  hashWithSalt s = hashWithSalt s . Array.assocs

--instance (Hashable i, Array.Ix i) => Hashable1 (Array i)
