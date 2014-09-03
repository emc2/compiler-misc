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

module Tests.Control.Monad.Symbols(tests) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Symbols
import Data.ByteString.Char8 hiding (zip)
import Data.Symbol
import Test.HUnitPlus.Base hiding (name)

unusedSym = firstSym
unusedName = pack "<unused>"
symA = succ unusedSym
nameA = pack "A"
symBB = succ symA
nameBB = pack "BB"
names = [unusedName, nameA, nameBB]
syms = [unusedSym, symA, symBB]
symAssocs = zip syms names
symBounds = (unusedSym, symBB)

testSymbolsProps :: MonadIO m => SymbolsT m ()
testSymbolsProps =
  do
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)

testReaderSymbolsAsk :: ReaderT Int (SymbolsT IO) ()
testReaderSymbolsAsk =
  do
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- ask
    liftIO (val @?= 0)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- ask
    liftIO (val @?= 0)

testReaderSymbolsLocal :: ReaderT Int (SymbolsT IO) ()
testReaderSymbolsLocal =
  let
    localtests :: ReaderT Int (SymbolsT IO) ()
    localtests =
      do
        unusedSym' <- nullSym
        liftIO (unusedSym' @?= unusedSym)
        names' <- allNames
        liftIO (names' @?= names)
        syms' <- allSyms
        liftIO (syms' @?= syms)
        nameA' <- name symA
        liftIO (nameA' @?= nameA)
        nameBB' <- name symBB
        liftIO (nameBB' @?= nameBB)
        val <- ask
        liftIO (val @?= 1)
        unusedSym' <- nullSym
        liftIO (unusedSym' @?= unusedSym)
        names' <- allNames
        liftIO (names' @?= names)
        syms' <- allSyms
        liftIO (syms' @?= syms)
        nameA' <- name symA
        liftIO (nameA' @?= nameA)
        nameBB' <- name symBB
        liftIO (nameBB' @?= nameBB)
        val <- ask
        liftIO (val @?= 1)
  in do
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- ask
    liftIO (val @?= 0)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- ask
    liftIO (val @?= 0)
    local (const 1) localtests
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- ask
    liftIO (val @?= 0)
    local (const 1) localtests
    val <- ask
    liftIO (val @?= 0)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)

testSymbolsReaderAsk :: SymbolsT (ReaderT Int IO) ()
testSymbolsReaderAsk =
  do
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- ask
    liftIO (val @?= 0)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- ask
    liftIO (val @?= 0)

testSymbolsReaderLocal :: SymbolsT (ReaderT Int IO) ()
testSymbolsReaderLocal =
  let
    localtests :: SymbolsT (ReaderT Int IO) ()
    localtests =
      do
        unusedSym' <- nullSym
        liftIO (unusedSym' @?= unusedSym)
        names' <- allNames
        liftIO (names' @?= names)
        syms' <- allSyms
        liftIO (syms' @?= syms)
        nameA' <- name symA
        liftIO (nameA' @?= nameA)
        nameBB' <- name symBB
        liftIO (nameBB' @?= nameBB)
        val <- ask
        liftIO (val @?= 1)
        unusedSym' <- nullSym
        liftIO (unusedSym' @?= unusedSym)
        names' <- allNames
        liftIO (names' @?= names)
        syms' <- allSyms
        liftIO (syms' @?= syms)
        nameA' <- name symA
        liftIO (nameA' @?= nameA)
        nameBB' <- name symBB
        liftIO (nameBB' @?= nameBB)
        val <- ask
        liftIO (val @?= 1)
  in do
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- ask
    liftIO (val @?= 0)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- ask
    liftIO (val @?= 0)
    local (const 1) localtests
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- ask
    liftIO (val @?= 0)
    local (const 1) localtests
    val <- ask
    liftIO (val @?= 0)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)

testStateSymbolsProps :: StateT Int (SymbolsT IO) ()
testStateSymbolsProps =
  do
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- get
    liftIO (val @?= 0)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- get
    liftIO (val @?= 0)
    put 1
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- get
    liftIO (val @?= 1)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    put 2
    val <- get
    liftIO (val @?= 2)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)

testSymbolsStateProps :: SymbolsT (StateT Int IO) ()
testSymbolsStateProps =
  do
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- get
    liftIO (val @?= 0)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- get
    liftIO (val @?= 0)
    put 1
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    val <- get
    liftIO (val @?= 1)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)
    put 2
    val <- get
    liftIO (val @?= 2)
    unusedSym' <- nullSym
    liftIO (unusedSym' @?= unusedSym)
    names' <- allNames
    liftIO (names' @?= names)
    syms' <- allSyms
    liftIO (syms' @?= syms)
    nameA' <- name symA
    liftIO (nameA' @?= nameA)
    nameBB' <- name symBB
    liftIO (nameBB' @?= nameBB)

basicTest :: IO ()
basicTest = runSymbolsT testSymbolsProps symBounds symAssocs

readerSymbolsAskTest :: IO ()
readerSymbolsAskTest = runSymbolsT (runReaderT testReaderSymbolsAsk 0)
                                   symBounds symAssocs

symbolsReaderAskTest :: IO ()
symbolsReaderAskTest = runReaderT (runSymbolsT testSymbolsReaderAsk
                                               symBounds symAssocs) 0

readerSymbolsLocalTest :: IO ()
readerSymbolsLocalTest = runSymbolsT (runReaderT testReaderSymbolsLocal 0)
                                     symBounds symAssocs

symbolsReaderLocalTest :: IO ()
symbolsReaderLocalTest = runReaderT (runSymbolsT testSymbolsReaderLocal
                                                 symBounds symAssocs) 0

stateSymbolsTest :: IO ()
stateSymbolsTest = runSymbolsT (runStateT testStateSymbolsProps 0)
                               symBounds symAssocs >>=
                   return . fst

symbolsStateTest :: IO ()
symbolsStateTest = runStateT (runSymbolsT testSymbolsStateProps
                                          symBounds symAssocs) 0 >>=
                   return . fst

testlist :: [Test]
testlist = [
    "basic" ~: basicTest,
    "Reader_Symbols_ask" ~: readerSymbolsAskTest,
    "Symbols_Reader_ask" ~: symbolsReaderAskTest,
    "Reader_Symbols_local" ~: readerSymbolsLocalTest,
    "Symbols_Reader_local" ~: symbolsReaderLocalTest,
    "State_Symbols" ~: stateSymbolsTest,
    "Symbols_State" ~: symbolsStateTest
  ]

tests :: Test
tests = "Symbols" ~: testlist
