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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Text.AlexHelper(
       AlexInternalState(..),
       AlexMonadAction,
       AlexActions(..),
       AlexResultHandlers(..),

       -- * Internal definitions used by Alex
       AlexPosn(..),
       AlexInput,
       alexInputPrevChar,
       alexGetByte,
       alexStartPos,
       alexMove,

       -- * Alex lexer actions
       alexGetInput,
       alexSetInput,
       alexGetStartCode,
       alexSetStartCode,
       alexGetUserState,
       alexSetUserState,
       mkAlexActions,
       andBegin,
       token
       ) where

import Control.Monad.State
import Control.Monad.Genpos.Class
import Data.Position
import Data.Int
import Data.Word (Word8)
import Prelude hiding (span)

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Internal as ByteString (w2c)
import qualified Data.ByteString as Strict

type Byte = Word8

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  ByteString.ByteString)        -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,_,cs) | ByteString.null cs = Nothing
                     | otherwise = let b   = ByteString.head cs
                                       cs' = ByteString.tail cs
                                       c   = ByteString.w2c b
                                       p'  = alexMove p c
                                    in p' `seq` cs' `seq` Just (b, (p', c, cs'))

data AlexPosn = AlexPn !Int !Int !Int deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

-- | Alex (with the monadUserState-bytestring wrapper) generates this
-- data structure, except named @AlexState @ with
-- @alex_ust :: AlexUserState@ (@AlexUserState@ is defined by user code).
--
-- You can instead do the following in your lexer to get the same effect:
--
-- > type AlexState = AlexInternalState AlexUserState
data AlexInternalState userstate =
  AlexState {
    alex_pos :: !AlexPosn,  -- position at current input location
    alex_inp :: ByteString.ByteString,      -- the current input
    alex_chr :: !Char,      -- the character before the input
    alex_scd :: !Int,        -- the current startcode
    alex_fname :: !Strict.ByteString,
    alex_ust :: !userstate -- AlexUserState will be defined in the user program
  }

alexGetInput :: MonadState (AlexInternalState us) m => m AlexInput
alexGetInput =
  do
    AlexState { alex_pos = pos, alex_chr = c, alex_inp = inp } <- get
    return (pos, c, inp)

alexSetInput :: MonadState (AlexInternalState us) m => AlexInput -> m ()
alexSetInput (pos, c, inp) =
  do
    st <- get
    put st { alex_pos = pos, alex_chr = c, alex_inp = inp }

alexGetUserState :: MonadState (AlexInternalState us) m => m us
alexGetUserState =
  do
    AlexState { alex_ust = ust } <- get
    return ust

alexSetUserState :: MonadState (AlexInternalState us) m => us -> m ()
alexSetUserState ust =
  do
    st <- get
    put st { alex_ust = ust }

alexGetStartCode :: MonadState (AlexInternalState us) m => m Int
alexGetStartCode =
  do
    AlexState { alex_scd = sc } <- get
    return sc

alexGetFileName :: MonadState (AlexInternalState us) m => m Strict.ByteString
alexGetFileName =
  do
    AlexState { alex_fname = fname } <- get
    return fname

alexSetStartCode :: MonadState (AlexInternalState us) m => Int -> m ()
alexSetStartCode sc =
  do
    st <- get
    put st { alex_scd = sc }

-- | Alex defines a type @AlexAction@, which in the monad wrappers, is
-- @AlexInput -> Int -> Alex result@.  This type allows the monad to
-- be generalized.
type AlexMonadAction m result =
  AlexPosn -> AlexPosn -> ByteString.ByteString -> m result

data AlexActions m result =
  AlexActions {
    -- | The value for the @alexMonadScan@ definition that alex generates.
    actAlexMonadScan :: m result,
    -- | The value for the @skip@ definition that alex generates.
    actSkip :: AlexMonadAction m result,
    -- | The value for the @begin@ definition that alex generates.
    actBegin :: Int -> AlexMonadAction m result
  }

-- | A set of handlers for the possible values of the @AlexResult@
-- datatype defined by alex.
data AlexResultHandlers m result =
  AlexResultHandlers {
    -- | Handler for @AlexEOF@.
    handleEOF :: m result,
    -- | Handler for @AlexError@
    handleError :: AlexInput -> m result,
    -- | Handler for @AlexSkip@.
    handleSkip :: AlexInput -> Int -> m result,
    -- | Handler for @AlexToken@.
    handleToken :: AlexInput -> Int -> AlexMonadAction m result -> m result
  }

getLen :: AlexPosn -> AlexPosn -> Int64
getLen (AlexPn start _ _) (AlexPn end _ _) = fromIntegral (start - end)

mkPosition :: (MonadGenpos m, MonadState (AlexInternalState us) m) =>
              AlexPosn -> AlexPosn -> m Position
mkPosition (AlexPn _ startline startcol) (AlexPn _ endline endcol) =
  do
    fname <- alexGetFileName
    span fname (fromIntegral startline) (fromIntegral startcol)
         (fromIntegral endline) (fromIntegral endcol)

mkAlexActions :: forall us m result .
                 (MonadState (AlexInternalState us) m, MonadGenpos m) =>
                 (AlexResultHandlers m result -> AlexInput -> Int -> m result)
              -- ^ A wrapper around the @alexScan@ function.  This
              -- should be a simple call to @alexScan@, followed by a
              -- case statement that calls the appropriate action in
              -- the 'AlexResultHandlers' structure.
              -> (ByteString.ByteString -> Position -> m result)
              -- ^ An action to be taken when a lexical error occurs.
              -> m result
              -- ^ An action to be taken at the end of input.
              -> AlexActions m result
mkAlexActions scanWrapper alexError alexEOF =
  let
    alexMonadScan :: m result
    alexMonadScan =
      do
        inp @ (startpos, _, str) <- alexGetInput
        sc <- alexGetStartCode
        scanWrapper AlexResultHandlers { handleEOF = alexEOF,
                                         handleError =
                                           \inp' @ (endpos, _, _) ->
                                           let
                                             len = getLen startpos endpos
                                             tokstr = ByteString.take len str
                                           in do
                                             alexSetInput inp'
                                             pos <- mkPosition startpos endpos
                                             alexError tokstr pos,
                                         handleSkip =
                                           \inp' _ ->
                                           do
                                             alexSetInput inp'
                                             alexMonadScan,
                                         handleToken =
                                           \inp' @ (endpos, _, _) _ action ->
                                           let
                                             len = getLen startpos endpos
                                             tokstr = ByteString.take len str
                                           in do
                                             alexSetInput inp'
                                             action startpos endpos tokstr
                                       } inp sc

    skip :: AlexMonadAction m result
    skip _ _ _ = alexMonadScan

    begin :: Int -> AlexMonadAction m result
    begin code _ _ _ =
      do
        alexSetStartCode code
        alexMonadScan
  in
    AlexActions { actAlexMonadScan = alexMonadScan, actSkip = skip,
                  actBegin = begin }

andBegin :: MonadState (AlexInternalState us) m =>
            AlexMonadAction m result -> Int -> AlexMonadAction m result
(action `andBegin` code) startpos endpos tokstr =
  do
    alexSetStartCode code
    action startpos endpos tokstr

token :: (MonadGenpos m, MonadState (AlexInternalState us) m) =>
         (ByteString.ByteString -> Position -> m token) ->
         AlexMonadAction m token
token t startpos endpos bstr =
  do
    pos <- mkPosition startpos endpos
    t bstr pos
