-- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
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

-- | This module contains datatypes representing ranges of integers.
-- This is essentially a basis for interval arithmetic.
module Data.Interval(
       -- * Datatypes
       Interval(..),
       Intervals,

       -- * Bounds
       lower,
       upper,

       -- * Utility Functions
       size
       ) where

import Control.Monad
import Data.Hashable
import Data.List hiding (span)
import Prelude hiding (span)

-- | A datatype representing a single interval
data Interval n =
    -- | An interval consisting of an inclusive range of numbers.
    -- Interval 1 2 contains both 1 and 2.
    Interval !n !n
    -- | An interval consisting of a single number.  Single x is
    -- equavalent to Interval x x.
  | Single !n
    -- | An interval (inclusively) lower-bounded by a number, with no
    -- upper-bound.  Min 0 denotes all positive integers, and zero.
  | Min !n
    -- | An interval (inclusively) upper-bounded by a number, with no
    -- lower-bound.  Max 0 denotes all negative integers, and zero.
  | Max !n
    deriving (Ord, Eq)

-- | A datatype representing a set of intervals.
newtype Intervals n = Intervals { intervals :: [Interval n] }
  deriving (Ord, Eq)

-- | Lower bound of a interval
lower :: Interval n -> Maybe n
lower (Single n) = Just n
lower (Interval n _) = Just n
lower (Min n) = Just n
lower (Max _) = Nothing

-- | Upper bound of a interval
upper :: Interval n -> Maybe n
upper (Single n) = Just n
upper (Interval _ n) = Just n
upper (Max n) = Just n
upper (Min _) = Nothing

-- | Get the size of a single interval.
size :: Integral n => Interval n -> Maybe n
size (Interval lo hi) = Just (hi - lo + 1)
size (Single _) = Just 1
size _ = Nothing

instance Show n => Show (Interval n) where
  show (Min n) = show n ++ " to +inf"
  show (Single n) = show n
  show (Interval n1 n2) = show n1 ++ " to " ++ show n2
  show (Max n) = "-inf to " ++ show n

instance Show n => Show (Intervals n) where
  show (Intervals { intervals = [] }) = "-inf to +inf"
  show (Intervals { intervals = is }) = show is

instance Hashable n => Hashable (Interval n) where
  hashWithSalt s (Interval n1 n2) =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` n1 `hashWithSalt` n2
  hashWithSalt s (Single n) = s `hashWithSalt` (2 :: Int) `hashWithSalt` n
  hashWithSalt s (Min n) = s `hashWithSalt` (3 :: Int) `hashWithSalt` n
  hashWithSalt s (Max n) = s `hashWithSalt` (4 :: Int) `hashWithSalt` n

instance Hashable n => Hashable (Intervals n) where
  hashWithSalt s Intervals { intervals = is } = hashWithSalt s is
