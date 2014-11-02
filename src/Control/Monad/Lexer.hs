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

module Control.Monad.Lexer(
       MonadCommentBuffer(..),
       MonadGenpos(..),
       MonadGensym(..),
       MonadPositions(..),
       MonadKeywords(..),
       MonadMessages(..),
       LexerT,
       Lexer,
       LexerNoCommentsT,
       LexerNoComments,
       LexerNoBufferT,
       LexerNoBuffer,
       LexerNoCommentsNoBufferT,
       LexerNoCommentsNoBuffer,
       runLexerT,
       runLexer,
       runLexerNoCommentsT,
       runLexerNoComments,
       runLexerNoBufferT,
       runLexerNoBuffer,
       runLexerNoCommentsNoBufferT,
       runLexerNoCommentsNoBuffer
       ) where

import Control.Monad.CommentBuffer
import Control.Monad.Genpos
import Control.Monad.Gensym
import Control.Monad.Keywords
import Control.Monad.Messages
import Control.Monad.SourceBuffer
import Control.Monad.SkipComments
import Control.Monad.Trans
import Data.ByteString
import Data.Position

-- | Lexer monad transformer
type LexerT tok m =
  (KeywordsT tok (CommentBufferT (SourceBufferT (GenposT (GensymT m)))))

-- | Lexer monad
type Lexer tok = LexerT tok IO

-- | Lexer monad transformer without comment buffering
type LexerNoCommentsT tok m a =
  (KeywordsT tok (SkipCommentsT (SourceBufferT (GenposT (GensymT m))))) a

-- | Lexer monad without comment buffering
type LexerNoComments tok a = LexerNoCommentsT tok IO a

-- | Lexer monad transformer without source buffering
type LexerNoBufferT tok m =
  (KeywordsT tok (CommentBufferT (GenposT (GensymT m))))

-- | Lexer monad without source buffering
type LexerNoBuffer tok = LexerNoBufferT tok IO

-- | Lexer monad transformer without source buffering
type LexerNoCommentsNoBufferT tok m =
  (KeywordsT tok (SkipCommentsT (GenposT (GensymT m))))

-- | Lexer monad without source buffering
type LexerNoCommentsNoBuffer tok = LexerNoCommentsNoBufferT tok IO

runLexerT :: MonadIO m => LexerT tok m a
          -> [(ByteString, Position -> tok)]
          -> m a
runLexerT lexer keywords =
  let
    commentbuffer = runKeywordsT lexer keywords
    srcbuf = runCommentBufferT commentbuffer
    genpos = runSourceBufferT srcbuf
    gensym  = startGenposT genpos
  in
    startGensymT gensym

runLexer :: Lexer tok a
         -> [(ByteString, Position -> tok)]
         -> IO a
runLexer = runLexerT

runLexerNoCommentsT :: MonadIO m => LexerNoCommentsT tok m a
                    -> [(ByteString, Position -> tok)]
                    -> m a
runLexerNoCommentsT lexer keywords =
  let
    commentbuffer = runKeywordsT lexer keywords
    srcbuf = runSkipCommentsT commentbuffer
    genpos = runSourceBufferT srcbuf
    gensym  = startGenposT genpos
  in
    startGensymT gensym

runLexerNoComments :: LexerNoComments tok a
                   -> [(ByteString, Position -> tok)]
                   -> IO a
runLexerNoComments = runLexerNoCommentsT

runLexerNoBufferT :: MonadIO m => LexerNoBufferT tok m a
                  -> [(ByteString, Position -> tok)]
                  -> m a
runLexerNoBufferT lexer keywords =
  let
    commentbuffer = runKeywordsT lexer keywords
    genpos = runCommentBufferT commentbuffer
    gensym  = startGenposT genpos
  in
    startGensymT gensym

runLexerNoBuffer :: LexerNoBuffer tok a
                 -> [(ByteString, Position -> tok)]
                 -> IO a
runLexerNoBuffer = runLexerNoBufferT

runLexerNoCommentsNoBufferT :: MonadIO m => LexerNoBufferT tok m a
                            -> [(ByteString, Position -> tok)]
                            -> m a
runLexerNoCommentsNoBufferT lexer keywords =
  let
    commentbuffer = runKeywordsT lexer keywords
    genpos = runCommentBufferT commentbuffer
    gensym  = startGenposT genpos
  in
    startGensymT gensym

runLexerNoCommentsNoBuffer :: LexerNoBuffer tok a
                           -> [(ByteString, Position -> tok)]
                           -> IO a
runLexerNoCommentsNoBuffer = runLexerNoBufferT
