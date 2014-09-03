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

module Tests.Data.Symbol(tests) where

import Control.Monad
import Data.Hashable
import Data.Symbol
import Test.HUnitPlus.Base

import qualified Data.HashSet as HashSet

symbolEnum :: [Symbol]
symbolEnum = take 1000 (iterate succ firstSym)

testNoDuplicates :: (Hashable a, Ord a) => [a] -> IO ()
testNoDuplicates =
  let
    foldfun accum val =
      do
        not (HashSet.member val accum) @? "Duplicate value"
        return (HashSet.insert val accum)
  in void . foldM foldfun HashSet.empty

testlist :: [Test]
testlist = [
    "succ_pred" ~: (firstSym @=? pred (succ firstSym)),
    "enum" ~: testNoDuplicates symbolEnum,
    "hash" ~: testNoDuplicates (map hash symbolEnum)
  ]

tests :: Test
tests = "Symbol" ~: testlist
