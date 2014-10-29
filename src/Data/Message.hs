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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             FunctionalDependencies #-}

-- | Defines a class for compiler messages and collections of compiler
-- messages.
module Data.Message(
       Severity(Internal, Error, Warning, Remark, Lint, Info),
       MessageContent(..),
       Message(..),
       Messages(..),
       messageContent,
       putMessageContent,
       putMessage,
       putMessages,
       putMessagesXML
       ) where

import Control.Monad.Positions.Class
import Control.Monad.SourceFiles.Class
import Control.Monad.Trans
import Data.ByteString(ByteString)
import Data.ByteString.Lazy.Char8(pack)
import Data.Hashable
import Data.Monoid
import Data.Position
import Data.PositionInfo
import Data.Word
import System.IO
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

import qualified Data.ByteString.Lazy as Lazy

-- | Datatype representing the severity category of a compiler
-- message.  Use of these is up to an individual compiler, and it is
-- not necessary to use them all.  The following list of examples is
-- provided as a general guideline:
--
--   * @Internal@: assertion failure
--   * @Error@: parse error, type check error
--   * @Warning@: unused symbol, use of deprecated function
--   * @Remark@: verbose code, recommend more optimizable alternative
--   * @Lint@: recommend use of standard idiom
--   * @Info@: success/failure of an optimization
--
-- The value @None@ should never be used for an individual message.
-- It is used solely as the 'mempty' value in a 'Monoid' instance.
data Severity =
    -- | Used to indicate the severity of an empty group of messages.
    None
    -- | Purely informative message, not implying any sort of
    -- criticism.  Should only be printed if a verbose option is give.
  | Info
    -- | A lint message.  Represents a purely stylistic criticism,
    -- less severe than a remark.  Should not be displayed unless an
    -- option is given.
  | Lint
    -- | A criticism.  Indicates code that may be inefficient,
    -- inhibiting optimization, or verbose, but not severe enough for
    -- a warning.
  | Remark
    -- | A warning.  Does not terminate compilation, unless an option
    -- is given.
  | Warning
    -- | An error.  Should terminate compilation.
  | Error
    -- | An internal compiler error.  Likely will terminate
    -- compilation immediately and report as much information as it
    -- can.
  | Internal
    deriving (Enum, Ord, Eq)

-- | The content in a message.  'Message' instances are translated
-- into this type prior to output.  This type depends in no way on the
-- contents of any monad.  Thus, it has an 'XmlPickler' and 'Show'
-- instance.
data MessageContent =
  MessageContent {
    -- | The severity of the message.
    msgSeverity :: !Severity,
    -- | The position of the message.
    msgPosition :: !(Maybe PositionInfo),
    -- | A unique text key representing the kind of the message.  This
    -- is intended for use in XML attributes and internationalization.
    msgKind :: !ByteString,
    -- | A brief description of the nature of the message.
    msgBrief :: !Lazy.ByteString,
    -- | A detailed description of the nature of the message.
    msgDetails :: !Lazy.ByteString,
    -- | The source code context of the message.
    msgContext :: !Lazy.ByteString
  }

-- | Class of types representing compiler messages.
class Message msg where
  -- | Get the severity of a message.
  severity :: msg -> Severity
  -- | Get the position to which the message relates.
  position :: msg -> Maybe Position
  -- | Get a unique text name for the error.  This is intended for use
  -- in XML attributes and internationalization.
  kind :: msg -> ByteString
  -- | Get a brief human-readable description of the error.
  brief :: msg -> Lazy.ByteString
  -- | Get a detailed human-readable description of the error.
  details :: msg -> Lazy.ByteString

-- | Class of types representing a collection of compiler messages.
class (Monoid msgs, Message msg) => Messages msg msgs | msgs -> msg where
  -- | Create a collection of messages from a single message.
  singleton :: msg -> msgs
  -- | Get all messages in the collection.
  messages :: msgs -> [msg]

  -- | Get the maximum severity in the collection.
  maxSeverity :: msgs -> Severity
  maxSeverity = mconcat . map severity . messages

-- | Translate a 'Message' into 'MessageContent'
messageContent :: (MonadPositions m, MonadSourceFiles m, Message msg) =>
                  msg -> m MessageContent
messageContent msg =
  do
    (pinfo, ctx) <-
      case position msg of
        Nothing -> return (Nothing, Lazy.empty)
        Just pos ->
          do
            pinfo <- positionInfo pos
            ctx <- sourceFileSpan (filepath pinfo) (fst (start pinfo))
                                  (fst (end pinfo))
            return (Just pinfo, Lazy.concat ctx)
    return MessageContent { msgSeverity = severity msg, msgKind = kind msg,
                            msgBrief = brief msg, msgDetails = details msg,
                            msgPosition = pinfo, msgContext = ctx }

putMessageContent :: Handle -> MessageContent -> IO ()
putMessageContent handle =
  let
    putbstr = Lazy.hPut handle
    putstr = hPutStr handle
    putln = hPutStr handle (show nativeNewline)

    putPosition Nothing = return ()
    putPosition (Just pos) = putstr (show pos)

    putSeverity Internal = putbstr (pack "Internal error")
    putSeverity Error = putbstr (pack "Error")
    putSeverity Warning = putbstr (pack "Warning")
    putSeverity Remark = putbstr (pack "Remark")
    putSeverity Lint = putbstr (pack "Lint warning")
    putSeverity Info = putbstr (pack "Info")
    putSeverity _ = error "Should not see a message with severity None"

    putMessageContent' MessageContent { msgSeverity = msev,
                                        msgPosition = mpos,
                                        msgBrief = mbrief,
                                        msgDetails = mdetails,
                                        msgContext = mctx } =
      do
        putSeverity msev
        putPosition mpos
        putstr ": "
        putbstr mbrief
        putln
        putbstr mctx
        putstr "  "
        putbstr mdetails
        putln
  in
    putMessageContent'

putMessage :: (MonadPositions m, MonadSourceFiles m, MonadIO m, Message msg) =>
               Handle -> msg -> m ()
putMessage handle msg =
  do
    contents <- messageContent msg
    liftIO (putMessageContent handle contents)

-- | Output a collection of messages to a given 'Handle' as text.
putMessages :: (MonadPositions m, MonadSourceFiles m, MonadIO m,
                Messages msg msgs, Message msg) =>
               Handle -> msgs -> m ()
putMessages handle = mapM_ (putMessage handle) . messages

-- | Output a collection of messages to a given 'Handle' as XML.
putMessagesXML :: (MonadPositions m, MonadSourceFiles m, MonadIO m,
                   Messages msg msgs, Message msg) =>
                  Handle -> msgs -> m ()
putMessagesXML handle msgs =
  let
    pickler :: PU (UNode ByteString) [MessageContent]
    pickler = xpRoot (xpElemNodes (gxFromString "messages") (xpList xpickle))
  in do
    contents <- mapM messageContent (messages msgs)
    liftIO (Lazy.hPut handle
                      (pickleXML pickler
                                 contents))
    liftIO (hPutStr handle (show nativeNewline))

instance Hashable Severity where
  hashWithSalt s Internal = s `hashWithSalt` (0 :: Word)
  hashWithSalt s Error = s `hashWithSalt` (1 :: Word)
  hashWithSalt s Warning = s `hashWithSalt` (2 :: Word)
  hashWithSalt s Remark = s `hashWithSalt` (3 :: Word)
  hashWithSalt s Lint = s `hashWithSalt` (4 :: Word)
  hashWithSalt s Info = s `hashWithSalt` (5 :: Word)
  hashWithSalt s None = s `hashWithSalt` (6 :: Word)

instance Hashable MessageContent where
  hashWithSalt s MessageContent { msgSeverity = msev, msgKind = mkind,
                                  msgPosition = mpos, msgBrief = mbrief,
                                  msgDetails = mdetails, msgContext = mctx } =
    s `hashWithSalt` msev `hashWithSalt` mkind `hashWithSalt`
    mpos `hashWithSalt` mbrief `hashWithSalt` mdetails `hashWithSalt` mctx

instance Monoid Severity where
  mempty = None

  mappend s1 s2
    | s1 < s2 = s1
    | otherwise = s2

instance Show Severity where
  show Internal = "Internal Error"
  show Error = "Error"
  show Warning = "Warning"
  show Remark = "Remark"
  show Lint = "Lint Warning"
  show Info = "Info"
  show None = "None"

instance (GenericXMLString tag, Show tag, GenericXMLString text) =>
         XmlPickler [(tag, text)] Severity where
  xpickle =
    let
      picker Internal = 0
      picker Error = 1
      picker Warning = 2
      picker Remark = 3
      picker Lint = 4
      picker Info = 5
      picker _ = error "Shouldn't see severity None"
    in
      xpAlt picker [xpWrap (const Internal, const ())
                           (xpAttrFixed (gxFromString "severity")
                                        (gxFromString "internal")),
                    xpWrap (const Error, const ())
                           (xpAttrFixed (gxFromString "severity")
                                        (gxFromString "error")),
                    xpWrap (const Warning, const ())
                           (xpAttrFixed (gxFromString "severity")
                                        (gxFromString "warning")),
                    xpWrap (const Remark, const ())
                           (xpAttrFixed (gxFromString "severity")
                                        (gxFromString "remark")),
                    xpWrap (const Lint, const ())
                           (xpAttrFixed (gxFromString "severity")
                                        (gxFromString "lint")),
                    xpWrap (const Error, const ())
                           (xpAttrFixed (gxFromString "severity")
                                        (gxFromString "info"))]


instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] MessageContent where
  xpickle =
    let
      posName = gxFromString "position"
      kindName = gxFromString "kind"
      briefName = gxFromString "brief"
      detailsName = gxFromString "details"
      ctxName = gxFromString "context"

      packStr bstr
        | bstr /= Lazy.empty = Just (gxFromByteString (Lazy.toStrict bstr))
        | otherwise = Nothing

      unpackStr (Just bstr) = Lazy.fromStrict (gxToByteString bstr)
      unpackStr Nothing = Lazy.empty

      packMsgContent MessageContent { msgSeverity = msev, msgContext = mctx,
                                      msgPosition = pos, msgBrief = mbrief,
                                      msgDetails = mdetails, msgKind = mkind } =
        ((msev, gxFromByteString mkind), (pos, packStr mbrief, packStr mdetails,
                                        packStr mctx))

      unpackMsgContent ((msev, mkind), (pos, mbrief, mdetails, mctx)) =
        MessageContent { msgSeverity = msev, msgContext = unpackStr mctx,
                         msgPosition = pos, msgBrief = unpackStr mbrief,
                         msgKind = gxToByteString mkind,
                         msgDetails = unpackStr mdetails }
    in
      xpWrap (unpackMsgContent, packMsgContent)
             (xpElem (gxFromString "message")
                     (xpPair xpickle (xpAttr kindName xpText))
                     (xp4Tuple (xpOption (xpElemNodes posName
                                                      xpickle))
                               (xpOption (xpElemNodes briefName
                                                      (xpContent xpText)))
                               (xpOption (xpElemNodes detailsName
                                                      (xpContent xpText)))
                               (xpOption (xpElemNodes ctxName
                                                      (xpContent xpText)))))
