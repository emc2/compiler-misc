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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, UndecidableInstances #-}

module Control.Monad.ScopeBuilder.Class(
       TempScope(..),
       MonadScopeStack(..),
       MonadScopeBuilder(..)
       ) where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Journal
import Control.Monad.Writer
import Data.Position.BasicPosition
import Data.ScopeID

-- | Typeclass for conversion from temporary to finalized scopes.  The
-- primary use for this is to allow multiple definitions to be build
-- up and then reported in a single error message.
class Monad m => TempScope tmpscope scope m | tmpscope -> scope where
  -- | Convert a temporary scope into its finalized form.
  finalizeScope :: tmpscope
                -- ^ The temporary scope.
                -> m scope
                -- ^ The finalized form.
  -- | Create a subscope from its parent scopes
  createSubscope ::  BasicPosition -> [tmpscope] -> m tmpscope

-- | Monad class defining a scope stack.
class Monad m => MonadScopeStack m where
  -- | Begin a new scope.  This will save the contents of the current
  -- scope and reset it to an empty state.
  enterScope :: BasicPosition -> m ()
  -- | Complete the current scope and return its scope ID.  Set the
  -- current state to the next lowest saved state.  If there are no
  -- remaining saved states, this will cause an error.
  finishScope :: m ScopeID

  makeScope :: BasicPosition -> m a -> m (a, ScopeID)
  makeScope pos action =
    do
      enterScope pos
      res <- action
      scopeid <- finishScope
      return (res, scopeid)

-- | Monad class for building up nested scopes.
class MonadScopeStack m => MonadScopeBuilder tmpscope m where
  -- | Get the current scope.
  getScope :: m tmpscope
  -- | Set the current scope.
  setScope :: tmpscope -> m ()

  -- | Alter the state of the current scope.
  updateScope :: (tmpscope -> tmpscope)
              -- ^ Action with which to update the scope.
              -> m ()
  updateScope func =
    do
      curr <- getScope
      setScope (func curr)

  -- | Alter the state of the current scope.
  updateScopeM :: (tmpscope -> m tmpscope)
              -- ^ Action with which to update the scope.
              -> m ()
  updateScopeM func =
    do
      curr <- getScope
      newscope <- func curr
      setScope newscope

  -- | Alter the state of the current scope and return a result.
  alterScope :: (tmpscope -> (a, tmpscope))
             -- ^ Action with which to update the scope.
             -> m a
  alterScope func =
    do
      curr <- getScope
      (out, newscope) <- return $! func curr
      setScope newscope
      return out

  -- | Alter the state of the current scope with a monadic action and
  -- return a result.
  alterScopeM :: (tmpscope -> m (a, tmpscope)) -> m a
  alterScopeM func =
    do
      curr <- getScope
      (out, newscope) <- func curr
      setScope newscope
      return out

instance MonadScopeStack m => MonadScopeStack (ContT r m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance (MonadScopeStack m) => MonadScopeStack (ExceptT e m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance (MonadScopeStack m) => MonadScopeStack (JournalT e m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance MonadScopeStack m => MonadScopeStack (ListT m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance MonadScopeStack m => MonadScopeStack (ReaderT r m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance MonadScopeStack m => MonadScopeStack (StateT s m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance (MonadScopeStack m, Monoid w) => MonadScopeStack (WriterT w m) where
  enterScope = lift . enterScope
  finishScope = lift finishScope

instance MonadScopeBuilder tmpscope m =>
         MonadScopeBuilder tmpscope (ContT r m) where
  getScope = lift getScope
  setScope = lift . setScope

instance (MonadScopeBuilder tmpscope m) =>
         MonadScopeBuilder tmpscope (ExceptT e m) where
  getScope = lift getScope
  setScope = lift . setScope

instance (MonadScopeBuilder tmpscope m) =>
         MonadScopeBuilder tmpscope (JournalT e m) where
  getScope = lift getScope
  setScope = lift . setScope

instance MonadScopeBuilder tmpscope m =>
         MonadScopeBuilder tmpscope (ListT m) where
  getScope = lift getScope
  setScope = lift . setScope

instance MonadScopeBuilder tmpscope m =>
         MonadScopeBuilder tmpscope (ReaderT r m) where
  getScope = lift getScope
  setScope = lift . setScope

instance MonadScopeBuilder tmpscope m =>
         MonadScopeBuilder tmpscope (StateT s m) where
  getScope = lift getScope
  setScope = lift . setScope

instance (MonadScopeBuilder tmpscope m, Monoid w) =>
         MonadScopeBuilder tmpscope (WriterT w m) where
  getScope = lift getScope
  setScope = lift . setScope
