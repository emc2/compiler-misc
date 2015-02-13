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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Defines a class of monads that describe a keyword table.  This is
-- used in lexers to implement more efficient keyword lexing.
module Control.Monad.Keywords.Class(
       MonadKeywords(..),
       keywordOrToken
       ) where

import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Gensym.Class
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString
import Data.Position
import Data.Symbol

import qualified Data.ByteString.Lazy as Lazy

-- | Class of monads encapsulating a keyword table.
class Monad m => MonadKeywords tok m where
  -- | Get information about a 'Position'
  mkKeyword :: ByteString
            -- ^ The text.
            -> Position
            -- ^ Start and end 'Position'.
            -> m (Maybe tok)

-- | Make a keyword token if the given text is a keyword; otherwise,
-- make a token using a supplied constructor.
keywordOrToken :: (MonadGensym m, MonadKeywords tok m) =>
                  (Symbol -> Position -> tok)
               -- ^ Function used to construct tokens.
               -> Lazy.ByteString
               -- ^ The text.
               -> Position
               -- ^ Position of the token.
               -> m tok
keywordOrToken func lazytext pos =
  let
    text = Lazy.toStrict lazytext
  in do
    keyword <- mkKeyword text pos
    case keyword of
      Just out -> return out
      Nothing ->
        do
          sym <- symbol text
          return (func sym pos)

instance MonadKeywords t m => MonadKeywords t (ContT r m) where
  mkKeyword p = lift . mkKeyword p

instance (MonadKeywords t m, Error e) => MonadKeywords t (ErrorT e m) where
  mkKeyword p = lift . mkKeyword p

instance MonadKeywords t m => MonadKeywords t (ListT m) where
  mkKeyword p = lift . mkKeyword p

instance MonadKeywords t m => MonadKeywords t (ReaderT r m) where
  mkKeyword p = lift . mkKeyword p

instance MonadKeywords t m => MonadKeywords t (StateT s m) where
  mkKeyword p = lift . mkKeyword p

instance (MonadKeywords t m, Monoid w) => MonadKeywords t (WriterT w m) where
  mkKeyword p = lift . mkKeyword p
