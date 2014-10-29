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
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, UndecidableInstances #-}

-- | Defines a monad class for reporting compiler messages.
module Control.Monad.Messages.Class(
       MonadMessages(..)
       ) where

import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Positions.Class
import Control.Monad.Reader
import Control.Monad.SourceFiles.Class
import Control.Monad.State
import Control.Monad.Writer
import Data.Message(Message, Severity)
import System.IO

import qualified Data.Message as Message

-- | Class of monads that can report compiler messages.
class (Message msg, Monad m) => MonadMessages msg m | m -> msg where
  -- | Report a complier message.
  message :: msg
          -- ^ Compiler message to report.
          -> m ()
  -- | Get all messages that have been logged.
  messages :: m [msg]

  -- | Get the highest severity among all messages.
  maxSeverity :: m Severity
  maxSeverity =
    do
      msgs <- messages
      return (maximum (map Message.severity msgs))

  -- | Output messages to a given 'Handle'.
  putMessages :: (MonadPositions m, MonadSourceFiles m, MonadIO m) =>
                 Handle -> m ()
  putMessages handle =
    do
      msgs <- messages
      Message.putMessages handle msgs

  -- | Output messages in XML format to a given 'Handle'.
  putMessagesXML :: (MonadPositions m, MonadSourceFiles m, MonadIO m) =>
                    Handle -> m ()
  putMessagesXML handle =
    do
      msgs <- messages
      Message.putMessagesXML handle msgs

instance MonadMessages msg m => MonadMessages msg (ContT r m) where
  message = lift . message
  messages = lift messages

instance (MonadMessages msg m, Error e) => MonadMessages msg (ErrorT e m) where
  message = lift . message
  messages = lift messages

instance MonadMessages msg m => MonadMessages msg (ListT m) where
  message = lift . message
  messages = lift messages

instance MonadMessages msg m => MonadMessages msg (ReaderT r m) where
  message = lift . message
  messages = lift messages

instance MonadMessages msg m => MonadMessages msg (StateT s m) where
  message = lift . message
  messages = lift messages

instance (MonadMessages msg m, Monoid w) =>
         MonadMessages msg (WriterT w m) where
  message = lift . message
  messages = lift messages
