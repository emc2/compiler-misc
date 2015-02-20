-- Copyright (c) 2014 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
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

-- | Defines a class of monads with the ability to create new
-- 'Position's.
module Control.Monad.Genpos.Class(
       MonadGenpos(..)
       ) where

import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Positions.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString
import Data.Position
import Data.PositionInfo
import Data.Word

-- | An extension to the 'MonadPositions' class that adds the ability
-- to create new 'Position's.
class MonadPositions m => MonadGenpos m where
  -- | Create a 'Position' from raw data.
  position :: PositionInfo
           -- ^ The name of the file.
           -> m Position

  -- | Create a span position from two existing positions.
  span :: Position -> Position -> m Position
  span startpos endpos =
    do
      startpinfo <- positionInfo startpos
      endpinfo <- positionInfo endpos
      if filepath startpinfo == filepath endpinfo
        then
          let
            fname = filepath startpinfo
            (startline, startcol) = start startpinfo
            (endline, endcol) = end endpinfo
          in
            between fname startline startcol endline endcol
        else error $! "Trying to combine positions from different files" ++
                      show (filepath startpinfo) ++ " and " ++
                      show (filepath endpinfo)

  -- | Create a span position from raw data.
  between :: ByteString
          -- ^ The file name
          -> Word
          -- ^ The starting line
          -> Word
          -- ^ The starting column
          -> Word
          -- ^ The ending line
          -> Word
          -- ^ The ending column
          -> m Position
  between fname startline startcol endline endcol
    | startline == endline && startcol == endcol =
       point fname startline startcol
    | otherwise =
      position Span { spanStartLine = startline, spanStartColumn = startcol,
                      spanEndLine = endline, spanEndColumn = endcol,
                      spanFile = fname }

  -- | Create a point position from raw data.
  point :: ByteString
        -- ^ The file name.
        -> Word
        -- ^ The line number.
        -> Word
        -- ^ The column number.
        -> m Position
  point fname line col = position Point { pointFile = fname, pointLine = line,
                                          pointColumn = col }

  -- | Create a file position from raw data.
  file :: ByteString
       -- ^ The file name.
       -> m Position
  file = position . File

  -- | Create a command line position from raw data.
  cmdLine :: m Position
  cmdLine = position CmdLine

  -- | Create a synthetic position from raw data.
  synthetic :: ByteString
            -- ^ The description.
            -> m Position
  synthetic = position . Synthetic

instance MonadGenpos m => MonadGenpos (ContT r m) where
  position = lift . position

instance (Error e, MonadGenpos m) => MonadGenpos (ErrorT e m) where
  position = lift . position

instance MonadGenpos m => MonadGenpos (ListT m) where
  position = lift . position

instance MonadGenpos m => MonadGenpos (ReaderT r m) where
  position = lift . position

instance MonadGenpos m => MonadGenpos (StateT s m) where
  position = lift . position

instance (Monoid w, MonadGenpos m) => MonadGenpos (WriterT w m) where
  position = lift . position
