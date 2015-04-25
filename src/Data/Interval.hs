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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | This module contains datatypes representing ranges of integers.
-- This is essentially a basis for interval arithmetic.
module Data.Interval(
       -- * Datatypes
       Interval(..),

       -- * Bounds
       lower,
       upper,

       -- * Math functions
       member,
       members,

       -- * Utility Functions
       size
       ) where

import Data.Hashable
import Data.List hiding (span)
import Prelude hiding (span)
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

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

-- | Check whether a number is a member of an interval.
member :: Integral n => Interval n -> n -> Bool
member (Interval lo hi) num = num >= lo && num <= hi
member (Single num) num' = num == num'
member (Min num) num' = num' >= num
member (Max num) num' = num' <= num

-- | Get all members of the interval.
members :: Enum n => Interval n -> [n]
members (Interval lo hi) = enumFromTo lo hi
members (Single num) = [num]
members (Min num) = enumFrom num
members (Max num) = iterate pred num

instance Show n => Show (Interval n) where
  show (Min n) = show n ++ " to +inf"
  show (Single n) = show n
  show (Interval n1 n2) = show n1 ++ " to " ++ show n2
  show (Max n) = "-inf to " ++ show n

instance Hashable n => Hashable (Interval n) where
  hashWithSalt s (Interval n1 n2) =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` n1 `hashWithSalt` n2
  hashWithSalt s (Single n) = s `hashWithSalt` (2 :: Int) `hashWithSalt` n
  hashWithSalt s (Min n) = s `hashWithSalt` (3 :: Int) `hashWithSalt` n
  hashWithSalt s (Max n) = s `hashWithSalt` (4 :: Int) `hashWithSalt` n

intervalPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text,
                    Show n, Read n) =>
                   PU [NodeG [] tag text] (Interval n)
intervalPickler =
  let
    revfunc (Interval lo hi) = ((lo, hi), ())
    revfunc int = error $! "Can't convert " ++ show int
  in
    xpWrap (\((lo, hi), ()) -> Interval lo hi, revfunc)
           (xpElem (gxFromString "Interval")
                   (xpPair (xpAttr (gxFromString "low") xpPrim)
                           (xpAttr (gxFromString "high") xpPrim))
                   xpUnit)

singlePickler :: (GenericXMLString tag, Show tag,
                  GenericXMLString text, Show text,
                  Show n, Read n) =>
                 PU [NodeG [] tag text] (Interval n)
singlePickler =
  let
    revfunc (Single val) = (val, ())
    revfunc int = error $! "Can't convert " ++ show int
  in
    xpWrap (\(val, ()) -> Single val, revfunc)
           (xpElem (gxFromString "Interval")
                   (xpAttr (gxFromString "value") xpPrim)
                   xpUnit)

minPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               Show n, Read n) =>
              PU [NodeG [] tag text] (Interval n)
minPickler =
  let
    revfunc (Min lo) = ((lo, ()), ())
    revfunc int = error $! "Can't convert " ++ show int
  in
    xpWrap (\((lo, ()), ()) -> Min lo, revfunc)
           (xpElem (gxFromString "Interval")
                   (xpPair (xpAttr (gxFromString "low") xpPrim)
                           (xpAttrFixed (gxFromString "high")
                                        (gxFromString "infinity")))
                   xpUnit)

maxPickler :: (GenericXMLString tag, Show tag,
               GenericXMLString text, Show text,
               Show n, Read n) =>
              PU [NodeG [] tag text] (Interval n)
maxPickler =
  let
    revfunc (Max hi) = (((), hi), ())
    revfunc int = error $! "Can't convert " ++ show int
  in
    xpWrap (\(((), hi), ()) -> Min hi, revfunc)
           (xpElem (gxFromString "Interval")
                   (xpPair (xpAttrFixed (gxFromString "low")
                                        (gxFromString "-infinity"))
                           (xpAttr (gxFromString "high") xpPrim))
                   xpUnit)

instance (GenericXMLString tag, Show tag,
          GenericXMLString text, Show text,
          Read n, Show n) =>
         XmlPickler [NodeG [] tag text] (Interval n) where
  xpickle =
    let
      picker Min {} = 0
      picker Max {} = 1
      picker Single {} = 2
      picker Interval {} = 2
    in
      xpAlt picker [minPickler, maxPickler, singlePickler, intervalPickler]
