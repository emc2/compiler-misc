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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.ScopeID(
       ScopeID,
       firstScopeID
       ) where

import Data.Array
import Data.Hashable
import Text.Format
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree(NodeG)

newtype ScopeID = ScopeID { scopeID :: Word }
  deriving (Eq, Ord, Ix)

firstScopeID :: ScopeID
firstScopeID = ScopeID { scopeID = 0 }

instance Hashable ScopeID where
  hashWithSalt s ScopeID { scopeID = n } = s `hashWithSalt` n

instance Enum ScopeID where
  succ = ScopeID . succ . scopeID
  pred = ScopeID . pred . scopeID
  toEnum = ScopeID . toEnum
  fromEnum = fromEnum . scopeID
  enumFromThen ScopeID { scopeID = n } = map ScopeID . enumFromThen n . scopeID
  enumFromTo ScopeID { scopeID = n } = map ScopeID . enumFromTo n . scopeID
  enumFromThenTo ScopeID { scopeID = n } ScopeID { scopeID = m } =
    map ScopeID . enumFromThenTo n m . scopeID

instance Format ScopeID where
  format = format . scopeID

instance (GenericXMLString tag, Show tag, GenericXMLString text) =>
         XmlPickler [NodeG [] tag text] ScopeID where
  xpickle = xpWrap (ScopeID, scopeID) (xpContent xpPrim)

instance (GenericXMLString tag, Show tag, GenericXMLString text) =>
         XmlPickler [(tag, text)] ScopeID where
  xpickle = xpWrap (ScopeID, scopeID) (xpAttr (gxFromString "scope-id") xpPrim)
