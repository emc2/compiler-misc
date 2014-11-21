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
       messageContentNoContext,
       putMessages,
       putMessagesNoContext,
       putMessageContentsXML,
       putMessagesXML,
       putMessagesXMLNoContext
       ) where

import Control.Monad.Positions.Class
import Control.Monad.SourceFiles.Class
import Control.Monad.Trans
import Data.ByteString(ByteString)
import Data.Hashable
import Data.Monoid hiding ((<>))
import Data.Position
import Data.PositionInfo
import Data.Word
import System.IO
import Text.Format
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict.UTF8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.UTF8 as Lazy.UTF8

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
    -- | A brief description of the nature of the message.
    msgBrief :: !Lazy.ByteString,
    -- | A detailed description of the nature of the message.
    msgDetails :: !Lazy.ByteString,
    -- | The source code context of the message.
    msgContext :: ![ByteString]
  }

-- | Class of types representing compiler messages.
class Message msg where
  -- | Get the severity of a message.
  severity :: msg -> Severity
  -- | Get the position to which the message relates.
  position :: msg -> Maybe Position
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

highlight :: Severity -> Doc -> Doc
highlight Internal doc = dullMagenta doc
highlight Error doc = dullRed doc
highlight Warning doc = dullYellow doc
highlight Remark doc = dullCyan doc
highlight Lint doc = dullBlue doc
highlight Info doc = dullGreen doc
highlight None doc = dullBlack doc

highlightVivid :: Severity -> Doc -> Doc
highlightVivid Internal doc = vividMagenta doc
highlightVivid Error doc = vividRed doc
highlightVivid Warning doc = vividYellow doc
highlightVivid Remark doc = vividCyan doc
highlightVivid Lint doc = vividBlue doc
highlightVivid Info doc = vividGreen doc
highlightVivid None doc = vividBlack doc

-- | Build a 'Doc' for context information that highlights the section
-- referenced by the 'PositionInfo'.
buildContext :: Severity -> PositionInfo -> [ByteString] -> Doc
buildContext sev Span { spanStartColumn = startcol, spanEndColumn = endcol }
             [ctx] =
  let
    lazy = Lazy.fromStrict ctx
    (pre, rest) = Lazy.UTF8.splitAt (fromIntegral $! startcol - 1) lazy
    (marked, post) = Lazy.UTF8.splitAt (fromIntegral $! endcol - startcol + 1)
                                       rest
  in
    cat [lazyBytestring pre,
         highlight sev (lazyBytestring marked),
         lazyBytestring post, line ]
buildContext sev Span { spanStartColumn = startcol, spanEndColumn = endcol }
             ctx =
  let
    firstline = head ctx
    middle = tail (init ctx)
    endline = last ctx
    lazyfirst = Lazy.fromStrict firstline
    lazyend = Lazy.fromStrict endline
    (pre, markedfirst) = Lazy.UTF8.splitAt (fromIntegral $! startcol - 1)
                                           lazyfirst
    (markedlast, post) = Lazy.UTF8.splitAt (fromIntegral $! endcol) lazyend
    markeddocs = lazyBytestring markedfirst : map bytestring middle ++
                 [lazyBytestring markedlast]
  in
    cat [lazyBytestring pre,
         highlight sev (vcat markeddocs),
         lazyBytestring post, line]
buildContext sev Point { pointColumn = col } [ctx] =
  let
    lazy = Lazy.fromStrict ctx
    (pre, rest) = Lazy.UTF8.splitAt (fromIntegral $! col - 1) lazy
  in case Lazy.UTF8.uncons rest of
    Just (chr, post) -> cat [lazyBytestring pre,
                             highlight sev (char chr),
                             lazyBytestring post, line]
    Nothing -> lazyBytestring pre <> highlight sev (lazyBytestring rest) <> line
buildContext _ _ [] = empty
buildContext _ _ _ = error "Impossible case"

-- | Translate a 'Message' into 'MessageContent'
messageContent :: (MonadPositions m, MonadSourceFiles m, Message msg) =>
                  msg -> m MessageContent
messageContent msg =
  do
    (pinfo, ctx) <-
      case position msg of
        Nothing -> return (Nothing, [])
        Just pos ->
          do
            pinfo <- positionInfo pos
            case pinfo of
              Span { spanFile = fname, spanStartLine = startline,
                     spanEndLine = endline } ->
                do
                  ctx <- sourceFileSpan fname startline endline
                  return (Just pinfo, ctx)
              Point { pointFile = fname, pointLine = startend } ->
                do
                  ctx <- sourceFileSpan fname startend startend
                  return (Just pinfo, ctx)
              _ -> return (Just pinfo, [])
    return MessageContent { msgSeverity = severity msg, msgBrief = brief msg,
                            msgDetails = details msg, msgPosition = pinfo,
                            msgContext = ctx }

-- | Translate a 'Message' into 'MessageContent', without getting
-- source context.
messageContentNoContext :: (MonadPositions m, Message msg) =>
                        msg -> m MessageContent
messageContentNoContext msg =
  do
    pinfo <-
      case position msg of
        Nothing -> return Nothing
        Just pos ->
          do
            pinfo <- positionInfo pos
            return (Just pinfo)
    return MessageContent { msgSeverity = severity msg, msgBrief = brief msg,
                            msgDetails = details msg, msgPosition = pinfo,
                            msgContext = [] }

formatMessageContent :: MessageContent -> Doc
formatMessageContent MessageContent { msgSeverity = msev,
                                      msgPosition = mpos,
                                      msgBrief = mbrief,
                                      msgDetails = mdetails,
                                      msgContext = mctx } =
  let
    (posdoc, ctxdoc) = case mpos of
      Just p -> case mctx of
        [] -> (string " at " <> format p, empty)
        _ -> (string " at " <> format p, buildContext msev p mctx)
      Nothing -> (empty, empty)

    detailsdoc =
      if Lazy.null mdetails
        then empty
        else indent 2 (lazyBytestring mdetails) <> line
  in
   cat [hcat [format msev, vividWhite posdoc, string ":"],
        softline, nest 2 (lazyBytestring mbrief),
        line, ctxdoc, detailsdoc ]

formatMessage :: (MonadPositions m, MonadSourceFiles m,
                  MonadIO m, Message msg) =>
                 msg -> m Doc
formatMessage msg =
  do
    contents <- messageContent msg
    return (formatMessageContent contents)


formatMessageNoContext :: (MonadPositions m, MonadIO m, Message msg) =>
                          msg -> m Doc
formatMessageNoContext msg =
  do
    contents <- messageContentNoContext msg
    return (formatMessageContent contents)

-- | Output a collection of messages to a given 'Handle' as text.
putMessages :: (MonadPositions m, MonadSourceFiles m, MonadIO m,
                Messages msg msgs, Message msg) =>
               Handle -> msgs -> m ()
putMessages handle msgs =
  do
    docs <- mapM formatMessage (messages msgs)
    liftIO (putOptimal handle 80 True (vcat docs))

-- | Output a collection of messages to a given 'Handle' as text.
putMessagesNoContext :: (MonadPositions m, MonadIO m,
                         Messages msg msgs, Message msg) =>
                        Handle -> msgs -> m ()
putMessagesNoContext handle msgs =
  do
    docs <- mapM formatMessageNoContext (messages msgs)
    liftIO (putOptimal handle 80 True (vcat docs))


putMessageContentsXML :: (MonadIO m) =>
                         Handle
                      -> [MessageContent]
                      -> m ()
putMessageContentsXML handle contents =
  let
    pickler :: PU (UNode ByteString) [MessageContent]
    pickler = xpRoot (xpElemNodes (gxFromString "messages") (xpList xpickle))
  in
    liftIO (Lazy.hPut handle (pickleXML pickler contents))

-- | Output a collection of messages to a given 'Handle' as XML.
putMessagesXML :: (MonadPositions m, MonadSourceFiles m, MonadIO m,
                   Messages msg msgs, Message msg) =>
                  Handle -> msgs -> m ()
putMessagesXML handle msgs =
  do
    contents <- mapM messageContent (messages msgs)
    putMessageContentsXML handle contents

-- | Output a collection of messages to a given 'Handle' as XML,
-- without source context strings.
putMessagesXMLNoContext :: (MonadPositions m, MonadIO m,
                            Messages msg msgs, Message msg) =>
                           Handle -> msgs -> m ()
putMessagesXMLNoContext handle msgs =
  do
    contents <- mapM messageContentNoContext (messages msgs)
    putMessageContentsXML handle contents

instance Message msg => Messages msg [msg] where
  singleton msg = [msg]
  messages = id

instance Hashable Severity where
  hashWithSalt s Internal = s `hashWithSalt` (0 :: Word)
  hashWithSalt s Error = s `hashWithSalt` (1 :: Word)
  hashWithSalt s Warning = s `hashWithSalt` (2 :: Word)
  hashWithSalt s Remark = s `hashWithSalt` (3 :: Word)
  hashWithSalt s Lint = s `hashWithSalt` (4 :: Word)
  hashWithSalt s Info = s `hashWithSalt` (5 :: Word)
  hashWithSalt s None = s `hashWithSalt` (6 :: Word)

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

instance Format Severity where
  format sev = highlightVivid sev (string (show sev))

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
      briefName = gxFromString "brief"
      detailsName = gxFromString "details"
      ctxName = gxFromString "context"

      packStr bstr
        | bstr /= Lazy.empty = Just (gxFromByteString (Lazy.toStrict bstr))
        | otherwise = Nothing

      unpackStr (Just bstr) = Lazy.fromStrict (gxToByteString bstr)
      unpackStr Nothing = Lazy.empty

      packStrict bstr
        | bstr /= Strict.empty = Just (gxFromByteString bstr)
        | otherwise = Nothing

      unpackStrict (Just bstr) = gxToByteString bstr
      unpackStrict Nothing = Strict.empty

      packMsgContent MessageContent { msgSeverity = msev, msgContext = mctx,
                                      msgPosition = pos, msgBrief = mbrief,
                                      msgDetails = mdetails } =
        (msev,
         (pos, packStr mbrief, packStr mdetails,
          packStrict (Strict.intercalate (Strict.UTF8.fromString "\n") mctx)))

      unpackMsgContent (msev, (pos, mbrief, mdetails, mctx)) =
        MessageContent { msgSeverity = msev,msgDetails = unpackStr mdetails,
                         msgContext = Strict.UTF8.lines (unpackStrict mctx),
                         msgPosition = pos, msgBrief = unpackStr mbrief }
    in
      xpWrap (unpackMsgContent, packMsgContent)
             (xpElem (gxFromString "message")
                     xpickle
                     (xp4Tuple (xpOption (xpElemNodes posName
                                                      xpickle))
                               (xpOption (xpElemNodes briefName
                                                      (xpContent xpText)))
                               (xpOption (xpElemNodes detailsName
                                                      (xpContent xpText)))
                               (xpOption (xpElemNodes ctxName
                                                      (xpContent xpText)))))
