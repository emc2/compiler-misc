-- Copyright (c) 2014, 2015, 2016 Eric McCorkle.  All rights reserved.
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
             FunctionalDependencies, FlexibleContexts #-}

-- | Defines a class for compiler messages and collections of compiler
-- messages.
module Data.Message(
       Severity(Internal, Error, Warning, Remark, Lint, Info),
       Highlighting(..),
       MessageContentPosition(..),
       MessageContent(..),
       Message(..),
       MessagePosition(..),
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
import System.IO
import Text.Format hiding (concat)
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict.UTF8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.UTF8 as Lazy.UTF8
import qualified Data.Position as Position

-- | Indicates how to highlight text in the message context.
data Highlighting =
    -- | Highlight foreground text.
    Foreground
    -- | Highlight background text.
  | Background

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

-- | A processed form of position information.
data MessageContentPosition =
    -- | A compound position, consisting of a base position, and then
    -- a list of child positions.
    Compound {
      -- | The base position.
      compoundBase :: MessageContentPosition,
      -- | A possible description relating the children to the base.
      compoundDesc :: !ByteString,
      -- | The child positions.
      compoundChildren :: [MessageContentPosition]
    }
  | Span {
      -- | A possible description.
      spanDesc :: !ByteString,
      -- | The file in which this position occurs.
      spanFile :: !ByteString,
      -- | The start line.
      spanStartLine :: !Word,
      -- | The starting column.
      spanStartCol :: !Word,
      -- | The ending line.
      spanEndLine :: !Word,
      -- | The ending column.
      spanEndCol :: !Word,
      -- | The context source lines.
      spanContext :: ![ByteString]
    }
  | Point {
      -- | A possible description.
      pointDesc :: !ByteString,
      -- | The file in which this position occurs.
      pointFile :: !ByteString,
      -- | The line at which this occurs.
      pointLine :: !Word,
      -- | The column at which this occurs.
      pointCol :: !Word,
      -- | The context source lines.
      pointContext :: ![ByteString]
    }
    -- | A message about a particular file.
  | File {
      -- | A possible description.
      fileDesc :: !ByteString,
      -- | The file name.
      fileName :: !ByteString
    }
    -- | A position with an arbitrary description, not from any file.
  | Other {
      -- | The description.
      otherDesc :: !ByteString
    }

-- | The content in a message.  'Message' instances are translated
-- into this type prior to output.  This type depends in no way on the
-- contents of any monad.  Thus, it has an 'XmlPickler' and 'Show'
-- instance.
data MessageContent =
  MessageContent {
    -- | The severity of the message.
    msgSeverity :: !Severity,
    -- | The position of the message.
    msgPositions :: [MessageContentPosition],
    -- | A brief description of the nature of the message.
    msgBrief :: !Doc,
    -- | A detailed description of the nature of the message.
    msgDetails :: !(Maybe Doc)
  }

-- | Class of types representing compiler messages.
class Message msg where
  -- | Get the severity of a message.
  severity :: msg -> Severity
  -- | Get a brief human-readable description of the error.
  brief :: msg -> Doc
  -- | Get a detailed human-readable description of the error.
  details :: msg -> Maybe Doc
  -- | Indicate how to highlight text.  The default is foreground, as
  -- that is almost always correct.
  highlighting :: msg -> Highlighting
  highlighting _ = Foreground

class Message msg => MessagePosition pos msg | msg -> pos where
  -- | Get the position to which the message relates.
  positions :: msg -> [pos]

-- | Class of types representing a collection of compiler messages.
class (Monoid msgs, Message msg) => Messages msg msgs | msgs -> msg where
  -- | Create a collection of messages from a single message.
  singleton :: msg -> msgs
  -- | Get all messages in the collection.
  messages :: msgs -> [msg]

  -- | Get the maximum severity in the collection.
  maxSeverity :: msgs -> Severity
  maxSeverity = mconcat . map severity . messages

highlight :: Highlighting -> Severity -> Doc -> Doc
highlight Foreground Internal doc = dullMagenta doc
highlight Background Internal doc = dullMagentaBackground doc
highlight Foreground Error doc = dullRed doc
highlight Background Error doc = dullRedBackground doc
highlight Foreground Warning doc = dullYellow doc
highlight Background Warning doc = dullYellowBackground doc
highlight Foreground Remark doc = dullCyan doc
highlight Background Remark doc = dullCyanBackground doc
highlight Foreground Lint doc = dullBlue doc
highlight Background Lint doc = dullBlueBackground doc
highlight Foreground Info doc = dullGreen doc
highlight Background Info doc = dullGreenBackground doc
highlight Foreground None doc = dullBlack doc
highlight Background None doc = dullBlackBackground doc

highlightVivid :: Severity -> Doc -> Doc
highlightVivid Internal doc = vividMagenta doc
highlightVivid Error doc = vividRed doc
highlightVivid Warning doc = vividYellow doc
highlightVivid Remark doc = vividCyan doc
highlightVivid Lint doc = vividBlue doc
highlightVivid Info doc = vividGreen doc
highlightVivid None doc = vividBlack doc

advance :: Char -> (Word, String) -> (Word, String)
advance '\t' (currcol, str) =
  let
    newcol = ((currcol + 7) `div` 8) * 8 + 1
    newstr = replicate (fromIntegral (newcol - currcol)) ' ' ++ str
  in
    (newcol, newstr)
advance chr (currcol, str) = (currcol + 1, chr : str)

splitTwo :: Word -> Lazy.ByteString -> (String, String)
splitTwo col bstr =
  let
    foldfun :: (Word, String, String) -> Char -> (Word, String, String)
    foldfun (off, pre', post') chr
      | off < col =
        let
          (newcol, newpre) = advance chr (off, pre')
        in
          (newcol, newpre, post')
      | otherwise =
        let
          (newcol, newpost) = advance chr (off, post')
        in
          (newcol, pre', newpost)
    (_, pre, post) = Lazy.UTF8.foldl foldfun (1, "", "") bstr
  in
    (reverse pre, reverse post)

splitThree :: Word -> Word -> Lazy.ByteString -> (String, String, String)
splitThree startcol endcol bstr =
  let
    foldfun :: (Word, String, String, String) -> Char ->
               (Word, String, String, String)
    foldfun (off, pre', mid', post') chr
      | off < startcol =
        let
          (newoff, newpre) = advance chr (off, pre')
        in
          (newoff, newpre, mid', post')
      | off > endcol =
        let
          (newoff, newpost) = advance chr (off, post')
        in
          (newoff, pre', mid', newpost)
      | otherwise =
        let
          (newoff, newmid) = advance chr (off, mid')
        in
          (newoff, pre', newmid, post')
    (_, pre, mid, post) = Lazy.UTF8.foldl foldfun (1, "", "", "") bstr
  in
    (reverse pre, reverse mid, reverse post)

-- | Translate a 'Message' into 'MessageContent'
messageContent :: (MonadPositions m, MonadSourceFiles m,
                   MessagePosition pos msg, Position.Position info pos,
                   Position.PositionInfo info) =>
                  msg -> m MessageContent
messageContent msg =
  let
    getPosition :: (MonadPositions m, MonadSourceFiles m,
                    Position.PositionInfo info) =>
                   info -> m MessageContentPosition
    getPosition info =
      do
        loc <- Position.location info
        msgpos <- case loc of
          Just (fname, Just (startpoint, endpoint)) ->
            do
              Position.FileInfo { Position.fileInfoName = fstr } <-
                fileInfo fname
              Position.PointInfo { Position.pointLine = startline,
                                   Position.pointColumn = startcol } <-
                pointInfo startpoint
              Position.PointInfo { Position.pointLine = endline,
                                   Position.pointColumn = endcol } <-
                pointInfo endpoint
              ctx <- sourceFileSpan fname startline endline
              if startline == endline && startcol == endcol
                then return Point { pointFile = fstr, pointLine = startline,
                                    pointCol = startcol, pointContext = ctx,
                                    pointDesc = Position.description info }
                else return Span { spanStartLine = startline,
                                   spanStartCol = startcol,
                                   spanEndLine = endline,
                                   spanEndCol = endcol,
                                   spanFile = fstr, spanContext = ctx,
                                   spanDesc = Position.description info }
          Just (fname, Nothing) ->
            do
              Position.FileInfo { Position.fileInfoName = fstr } <-
                fileInfo fname
              return File { fileDesc = Position.description info,
                            fileName = fstr }
          Nothing -> return Other { otherDesc = Position.description info }
        case Position.children info of
          Nothing -> return msgpos
          Just (desc, infos) ->
            do
              nodes <- mapM getPosition infos
              return Compound { compoundBase = msgpos, compoundDesc = desc,
                                compoundChildren = nodes }
    poslist = positions msg
    infolist = concat (map Position.positionInfo poslist)
  in do
    posdata <- mapM getPosition infolist
    return MessageContent { msgSeverity = severity msg,
                            msgBrief = brief msg,
                            msgDetails = details msg,
                            msgPositions = posdata }

-- | Translate a 'Message' into 'MessageContent', without getting
-- source context.
messageContentNoContext :: (MonadPositions m, MessagePosition pos msg,
                            Position.Position info pos,
                            Position.PositionInfo info) =>
                           msg -> m MessageContent
messageContentNoContext msg =
  let
    getPosition :: (MonadPositions m, Position.PositionInfo info) =>
                   info -> m MessageContentPosition
    getPosition info =
      do
        loc <- Position.location info
        msgpos <- case loc of
          Just (fname, Just (startpoint, endpoint)) ->
            do
              Position.FileInfo { Position.fileInfoName = fstr } <-
                fileInfo fname
              Position.PointInfo { Position.pointLine = startline,
                                   Position.pointColumn = startcol } <-
                pointInfo startpoint
              Position.PointInfo { Position.pointLine = endline,
                                   Position.pointColumn = endcol } <-
                pointInfo endpoint
              if startline == endline && startcol == endcol
                then return Point { pointFile = fstr, pointLine = startline,
                                    pointCol = startcol, pointContext = [],
                                    pointDesc = Position.description info }
                else return Span { spanStartLine = startline,
                                   spanStartCol = startcol,
                                   spanEndLine = endline,
                                   spanEndCol = endcol,
                                   spanFile = fstr, spanContext = [],
                                   spanDesc = Position.description info }
          Just (fname, Nothing) ->
            do
              Position.FileInfo { Position.fileInfoName = fstr } <-
                fileInfo fname
              return File { fileDesc = Position.description info,
                            fileName = fstr }
          Nothing -> return Other { otherDesc = Position.description info }
        case Position.children info of
          Nothing -> return msgpos
          Just (desc, infos) ->
            do
              nodes <- mapM getPosition infos
              return Compound { compoundBase = msgpos, compoundDesc = desc,
                                compoundChildren = nodes }
    poslist = positions msg
    infolist = concat (map Position.positionInfo poslist)
  in do
    posdata <- mapM getPosition infolist
    return MessageContent { msgSeverity = severity msg,
                            msgBrief = brief msg,
                            msgDetails = details msg,
                            msgPositions = posdata }

formatMessageContent :: Highlighting -> MessageContent -> Doc
formatMessageContent hlight MessageContent { msgSeverity = sev,
                                             msgPositions = poslist,
                                             msgBrief = mbrief,
                                             msgDetails = mdetails  } =
  let
    -- Format the position
    formatPos depth Compound { compoundBase = base, compoundDesc = desc,
                               compoundChildren = children } =
      let
        newdepth = depth + 2
      in
        formatPos depth base <$$>
        nest newdepth (vcat (format desc : map (formatPos newdepth) children))
    formatPos depth Span { spanStartLine = startline, spanStartCol = startcol,
                           spanEndLine = endline, spanEndCol = endcol,
                           spanFile = fname, spanDesc = desc,
                           spanContext = ctx } =
      let
        ctxdoc = case ctx of
          [] -> empty
          -- For one line, split out the highlighted part
          [oneline] ->
            let
              lazy = Lazy.fromStrict oneline
              (pre, mid, post) = splitThree startcol endcol lazy
            in
              hcat [string pre,
                    highlight hlight sev (string mid),
                    string post, line ]
          -- For multiple lines, the formatting is more complex
          _ ->
            let
              -- Split off the first and last lines
              firstline = Lazy.fromStrict (head ctx)
              lastline = Lazy.fromStrict (last ctx)
              -- Split out the highlighted parts of the first and last lines
              (startpre, startpost) = splitTwo startcol firstline
              (endpre, endpost) = splitTwo (endcol + 1) lastline
              -- Split out the middle
              middle = tail (init ctx)
              -- Possibly shorten the middle portion if it is too long
              middlelen = length middle
              trimmedmiddle =
                if middlelen > 10
                  then take 3 middle ++ [Strict.UTF8.fromString "..."] ++
                       drop (middlelen - 3) middle
                  else middle
              -- Recombine all the highlighted portions
              markeddocs = string startpost : map bytestring trimmedmiddle ++
                           [string endpre]
            in
              hcat [string startpre,
                    highlight hlight sev (vcat markeddocs),
                    string endpost, line]

        locationdoc
          | startline == endline =
            vividWhite (hcat [bytestring fname, colon,
                              format startline, dot, format startcol,
                              char '-', format endcol ])
          | otherwise =
            vividWhite (hcat [bytestring fname, colon,
                              format startline, dot, format startcol,
                              char '-', format endline, dot, format endcol])
      in
        if Strict.null desc
          then nest depth (string "at " <> locationdoc) <$$> ctxdoc
          else nest depth (bytestring desc <+> string "at " <>
                           locationdoc) <$$> ctxdoc
    formatPos depth Point { pointLine = pointline, pointCol = pointcol,
                            pointFile = fname, pointDesc = desc,
                            pointContext = ctx } =
      let
        ctxdoc = case ctx of
          [] -> empty
          [oneline] ->
            let
              lazy = Lazy.fromStrict oneline
              (pre, mid, post) = splitThree pointcol pointcol lazy
            in
              cat [string pre,
                   highlight hlight sev (string mid),
                   string post, line]
          _ -> error "Shouldn't see multi-line context with a Point position"

        locationdoc = vividWhite (hcat [bytestring fname, colon,
                                        format pointline, dot, format pointcol])
      in
        if Strict.null desc
          then nest depth (string "at " <> locationdoc) <$$> ctxdoc
          else nest depth (bytestring desc <+>
                           string "at " <> locationdoc) <$$> ctxdoc
    formatPos depth File { fileName = fname, fileDesc = desc } =
      nest depth (bytestring desc <+> string "in " <>
                  vividWhite (bytestring fname))
    formatPos depth Other { otherDesc = desc } = nest depth (bytestring desc)

    posdocs = map (formatPos 0) poslist
  in case posdocs of
    [] -> case mdetails of
      Nothing -> nest 2 (format sev <> colon </> mbrief) <> line
      Just content -> nest 2 (format sev <> colon </> mbrief) <$$>
                      indent 2 content <> line
    [posdoc] -> case mdetails of
      Nothing -> format sev <> colon </> nest 2 mbrief </> posdoc <> line
      Just content -> format sev <> colon </> nest 2 mbrief </> posdoc <$$>
                      indent 2 content <> line
    _ -> case mdetails of
      Nothing -> format sev <> colon </> nest 2 mbrief <$$> vcat posdocs <> line
      Just content -> format sev <> colon </> nest 2 mbrief <$$>
                      vcat posdocs <$$> indent 2 content <> line

formatMessage :: (MonadPositions m, MonadSourceFiles m,
                  MonadIO m, MessagePosition pos msg,
                  Position.Position info pos,
                  Position.PositionInfo info) => msg -> m Doc
formatMessage msg =
  do
    contents <- messageContent msg
    return $! formatMessageContent (highlighting msg) contents


formatMessageNoContext :: (MonadPositions m, MonadIO m,
                           MessagePosition pos msg,
                           Position.Position info pos,
                           Position.PositionInfo info) =>
                          msg -> m Doc
formatMessageNoContext msg =
  do
    contents <- messageContentNoContext msg
    return $! formatMessageContent (highlighting msg) contents

-- | Output a collection of messages to a given 'Handle' as text.
putMessages :: (MonadPositions m, MonadSourceFiles m, MonadIO m,
                Messages msg msgs, MessagePosition pos msg,
                Position.Position info pos, Position.PositionInfo info) =>
               Handle -> msgs -> m ()
putMessages handle msgs =
  do
    docs <- mapM formatMessage (messages msgs)
    liftIO (putGreedy handle 4 80 True (vcat docs))

-- | Output a collection of messages to a given 'Handle' as text.
putMessagesNoContext :: (MonadPositions m, MonadIO m, Messages msg msgs,
                         MessagePosition pos msg, Position.Position info pos,
                         Position.PositionInfo info) =>
                        Handle -> msgs -> m ()
putMessagesNoContext handle msgs =
  do
    docs <- mapM formatMessageNoContext (messages msgs)
    liftIO (putGreedy handle 4 80 True (vcat docs))

putMessageContentsXML :: MonadIO m => Handle -> [MessageContent] -> m ()
putMessageContentsXML handle contents =
  let
    pickler :: PU (UNode ByteString) [MessageContent]
    pickler = xpRoot (xpElemNodes (gxFromString "messages") (xpList xpickle))
  in
    liftIO (Lazy.hPut handle (pickleXML pickler contents))

-- | Output a collection of messages to a given 'Handle' as XML.
putMessagesXML :: (MonadPositions m, MonadSourceFiles m, MonadIO m,
                   Messages msg msgs, MessagePosition pos msg,
                   Position.Position info pos, Position.PositionInfo info) =>
                  Handle -> msgs -> m ()
putMessagesXML handle msgs =
  do
    contents <- mapM messageContent (messages msgs)
    putMessageContentsXML handle contents

-- | Output a collection of messages to a given 'Handle' as XML,
-- without source context strings.
putMessagesXMLNoContext :: (MonadPositions m, MonadIO m, Messages msg msgs,
                            MessagePosition pos msg, Position.Position info pos,
                            Position.PositionInfo info) =>
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

compoundPickler :: (GenericXMLString tag, Show tag,
                    GenericXMLString text, Show text) =>
                   PU [NodeG [] tag text] MessageContentPosition
compoundPickler =
  let
    fwdfunc (base, desc, children) =
      Compound { compoundBase = base, compoundDesc = gxToByteString desc,
                 compoundChildren = children }

    revfunc Compound { compoundBase = base, compoundDesc = desc,
                       compoundChildren = children } =
      (base, gxFromByteString desc, children)
    revfunc _ = error $! "Can't convert to Compound"
  in
    xpWrap (fwdfunc, revfunc)
           (xpElemNodes (gxFromString "compound")
                        (xpTriple (xpElemNodes (gxFromString "base") xpickle)
                                  (xpElemNodes (gxFromString "desc")
                                               (xpContent xpText))
                                  (xpElemNodes (gxFromString "children")
                                               (xpList xpickle))))

packContext :: (GenericXMLString text, Show text) => [ByteString] -> Maybe text
packContext [] = Nothing
packContext ctx =
  Just (gxFromByteString (Strict.intercalate (Strict.UTF8.fromString "\n") ctx))

unpackContext :: (GenericXMLString text, Show text) =>
                 Maybe text -> [ByteString]
unpackContext Nothing = []
unpackContext (Just txt) = Strict.UTF8.lines (gxToByteString txt)

spanPickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] MessageContentPosition
spanPickler =
  let
    fwdfunc ((fname, startline, startcol, endline, endcol), (ctx, Just desc)) =
      Span { spanStartLine = startline, spanStartCol = startcol,
             spanEndLine = endline, spanEndCol = endcol,
             spanFile = gxToByteString fname,
             spanDesc = gxToByteString desc,
             spanContext = unpackContext ctx }
    fwdfunc ((fname, startline, startcol, endline, endcol), (ctx, Nothing)) =
      Span { spanStartLine = startline, spanStartCol = startcol,
             spanEndLine = endline, spanEndCol = endcol,
             spanFile = gxToByteString fname,
             spanContext = unpackContext ctx,
             spanDesc = Strict.empty }

    revfunc Span { spanStartLine = startline, spanStartCol = startcol,
                   spanEndLine = endline, spanEndCol = endcol,
                   spanFile = fname, spanDesc = desc, spanContext = ctx } =
      ((gxFromByteString fname, startline, startcol, endline, endcol),
       (if Strict.null desc then Nothing else Just (gxFromByteString desc),
        packContext ctx))
    revfunc _ = error $! "Can't convert to Span"
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "span")
                   (xp5Tuple (xpAttr (gxFromString "file") xpText)
                             (xpAttr (gxFromString "start-line") xpPrim)
                             (xpAttr (gxFromString "start-column") xpPrim)
                             (xpAttr (gxFromString "end-line") xpPrim)
                             (xpAttr (gxFromString "end-column") xpPrim))
                   (xpPair (xpOption (xpElemNodes (gxFromString "desc")
                                                  (xpContent xpText)))
                           (xpOption (xpElemNodes (gxFromString "context")
                                                  (xpContent xpText)))))

pointPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] MessageContentPosition
pointPickler =
  let
    fwdfunc ((fname, pointline, pointcol), (Just desc, ctx)) =
      Point { pointLine = pointline, pointCol = pointcol,
              pointContext = unpackContext ctx,
              pointFile = gxToByteString fname,
              pointDesc = gxToByteString desc }
    fwdfunc ((fname, pointline, pointcol), (Nothing, ctx)) =
      Point { pointLine = pointline, pointCol = pointcol,
              pointContext = unpackContext ctx,
              pointFile = gxToByteString fname,
              pointDesc = Strict.empty }

    revfunc Point { pointLine = pointline, pointCol = pointcol,
                    pointFile = fname, pointDesc = desc, pointContext = ctx } =
      ((gxFromByteString fname, pointline, pointcol),
       (if Strict.null desc then Nothing else Just (gxFromByteString desc),
        packContext ctx))
    revfunc _ = error $! "Can't convert to Span"
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "point")
                   (xpTriple (xpAttr (gxFromString "file") xpText)
                             (xpAttr (gxFromString "line") xpPrim)
                             (xpAttr (gxFromString "column") xpPrim))
                   (xpPair (xpOption (xpElemNodes (gxFromString "desc")
                                                  (xpContent xpText)))
                           (xpOption (xpElemNodes (gxFromString "context")
                                                  (xpContent xpText)))))

filePickler :: (GenericXMLString tag, Show tag,
                GenericXMLString text, Show text) =>
               PU [NodeG [] tag text] MessageContentPosition
filePickler =
  let
    fwdfunc (fname, Just desc) =
      File { fileName = gxToByteString fname, fileDesc = gxToByteString desc }
    fwdfunc (fname, Nothing) =
      File { fileName = gxToByteString fname, fileDesc = Strict.empty }

    revfunc File { fileName = fname, fileDesc = desc } =
      (gxFromByteString fname,
       if Strict.null desc then Nothing else Just (gxFromByteString desc))
    revfunc _ = error $! "Can't convert to Span"
  in
    xpWrap (fwdfunc, revfunc)
           (xpElem (gxFromString "file")
                   (xpAttr (gxFromString "name") xpText)
                   (xpOption (xpElemNodes (gxFromString "desc")
                                          (xpContent xpText))))

otherPickler :: (GenericXMLString tag, Show tag,
                 GenericXMLString text, Show text) =>
                PU [NodeG [] tag text] MessageContentPosition
otherPickler =
  let
    fwdfunc desc = Other { otherDesc = gxToByteString desc }

    revfunc Other { otherDesc = desc } = gxFromByteString desc
    revfunc _ = error $! "Can't convert to Other"
  in
    xpWrap (fwdfunc, revfunc)
           (xpElemNodes (gxFromString "other")
                        (xpElemNodes (gxFromString "desc") (xpContent xpText)))

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] MessageContentPosition where
  xpickle =
    let
      picker Compound {} = 0
      picker Span {} = 1
      picker Point {} = 2
      picker File {} = 3
      picker Other {} = 4
    in
      xpAlt picker [compoundPickler, spanPickler, pointPickler,
                    filePickler, otherPickler]

instance (GenericXMLString tag, Show tag, GenericXMLString text, Show text) =>
         XmlPickler [NodeG [] tag text] MessageContent where
  xpickle =
    let
      posName = gxFromString "position"
      briefName = gxFromString "brief"
      detailsName = gxFromString "details"

      packDoc = gxFromByteString . Lazy.toStrict . renderGreedy 4 80 False

      packMaybeDoc (Just doc) = Just (packDoc doc)
      packMaybeDoc Nothing = Nothing

      unpackMaybeDoc (Just bstr) = Just (bytestring (gxToByteString bstr))
      unpackMaybeDoc Nothing = Nothing

      packMsgContent MessageContent { msgSeverity = msev,
                                      msgPositions = poslist,
                                      msgBrief = mbrief,
                                      msgDetails = mdetails } =
        (msev, (poslist, packDoc mbrief, packMaybeDoc mdetails))

      unpackMsgContent (msev, (poslist, mbrief, mdetails)) =
        MessageContent { msgSeverity = msev,
                         msgDetails = unpackMaybeDoc mdetails,
                         msgBrief = bytestring (gxToByteString mbrief),
                         msgPositions = poslist }
    in
      xpWrap (unpackMsgContent, packMsgContent)
             (xpElem (gxFromString "message")
                     xpickle
                     (xpTriple (xpElemNodes posName (xpList xpickle))
                               (xpElemNodes briefName (xpContent xpText))
                               (xpOption (xpElemNodes detailsName
                                                      (xpContent xpText)))))
