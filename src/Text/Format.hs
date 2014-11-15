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
{-# OPTIONS_GHC -Wall -Werror -funbox-strict-fields #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | A pretty printer implementation, based loosely on the
-- Wadler-Leijin pretty printer, but redesigned to facilitate a
-- dynamic programming optimal layout algorithm.
--
-- This pretty printer module trades some of the generality of the
-- Wadler-Leijin scheme in order to facilitate an efficient optimizing
-- layout engine.  The nesting, column, and width combinators are
-- removed.
module Text.Format(
       -- * Basic Definitions
       -- ** Types
       Doc,
       Graphics(..),
       -- ** Type Classes
       Format(..),

       -- * Creating @Doc@s

       -- ** Constructors

       -- *** Basic
       empty,
       line,
       linebreak,
       softline,
       softbreak,

       -- *** From datatypes
       char,
       string,
       bytestring,
       lazyBytestring,

       -- *** Literals
       lparen,
       rparen,
       lbrack,
       rbrack,
       lbrace,
       rbrace,
       langle,
       rangle,
       squote,
       dquote,
       backquote,
       comma,
       semi,
       colon,
       dot,
       backslash,
       equals,

       -- *** Derived
       nest,
       align,
       squoted,
       dquoted,
       parens,
       brackets,
       braces,
       angles,

       -- *** Graphics Mode
       graphics,
       dullWhite,
       dullRed,
       dullYellow,
       dullGreen,
       dullBlue,
       dullCyan,
       dullMagenta,
       dullBlack,
       vividWhite,
       vividRed,
       vividYellow,
       vividGreen,
       vividBlue,
       vividCyan,
       vividMagenta,
       vividBlack,

       -- ** Combining @Doc@s

       -- *** Basic
       beside,
       concat,
       choose,

       -- *** Derived
       (<>),
       (<+>),
       (<$>),
       (<$$>),
       (</>),
       (<//>),
       hsep,
       hcat,
       vsep,
       vcat,
       sep,
       cat,
       fillSep,
       fillCat,
       enclose,
       punctuate,
       encloseSep,

       -- ** Transforming @Doc@s
       flatten,
       group,

       -- * Rendering @Doc@s
       renderOneLine,
       renderFast,
       renderDynamic
       ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.List(intersperse, minimumBy)
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Word
import Prelude hiding (concat)
import System.Console.ANSI

import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as Strict.UTF8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified Data.ByteString.Lazy.UTF8 as Lazy.UTF8
import qualified Data.HashMap.Strict as HashMap

data Doc =
    -- | An empty document.
    Empty
    -- | A single character.  Cannot be a newline
  | Char { charContent :: !Char }
    -- | A ByteString.
  | Bytestring {
      -- | The length of the text
      bsLength :: !Int,
      -- | The content of the text
      bsContent :: !Strict.ByteString
    }
    -- | A Lazy ByteString
  | LazyBytestring {
      -- | The length of the text
      lbsLength :: !Int,
      -- | The content of the text
      lbsContent :: !Lazy.ByteString
    }
    -- | A line.
  | Line {
      -- | Whether to insert a space when undone by a group.
      insertSpace :: !Bool
    }
    -- | Concatenated documents.
  | Cat {
      catDocs :: [Doc]
    }
    -- | Increase the nesting level of a document.
  | Nest {
      -- | Amount by which to increase nesting.
      nestLevel :: !Int,
      -- | Document whose nesting should be increased.
      nestDoc :: Doc
    }
    -- | Choose the best from among a list of options.
  | Choose {
      -- | The list of options.
      chooseOptions :: [Doc]
    }
    -- | Set the nesting level of a document to the current column.
  | Align {
      alignDoc :: Doc
    }
    -- | Set graphics mode options when rendering this doc.
  | Graphics {
      graphicsSGR :: !Graphics,
      graphicsDoc :: Doc
    }

-- | Graphics options for ANSI terminals.
data Graphics =
    -- | Set options on the terminal, or keep the current setting in
    -- the case of 'Nothing'.
    Options {
      consoleIntensity :: !(Maybe ConsoleIntensity),
      underlining :: !(Maybe Underlining),
      blinkSpeed :: !(Maybe BlinkSpeed),
      foreground :: !(Maybe (Color, ColorIntensity)),
      background :: !(Maybe (Color, ColorIntensity)),
      swapForegroundBackground :: !(Maybe Bool)
    }
    -- | Reset the terminal in this mode.
  | Default

-- | Generate a 'Doc' representing a graphics mode switch.
switchGraphics :: Graphics -> Graphics -> Builder
switchGraphics _ Default = fromString (setSGRCode [Reset])
switchGraphics Default Options { consoleIntensity = consIntensity,
                                 swapForegroundBackground = swap,
                                 underlining = underline,
                                 foreground = fore,
                                 background = back,
                                 blinkSpeed = blink } =
  let
    withConsIntensity = maybe [] ((: []) . SetConsoleIntensity) consIntensity
    withUnderline = maybe withConsIntensity ((: withConsIntensity) .
                                             SetUnderlining)
                          underline
    withBlink = maybe withUnderline ((: withUnderline) . SetBlinkSpeed) blink
    withSwap = maybe withBlink ((: withBlink) . SetSwapForegroundBackground)
                     swap
    withForeground =
      maybe withSwap (\(color, intensity) -> SetColor Foreground intensity
                                             color : withSwap) fore
    withBackground =
      maybe withForeground (\(color, intensity) -> SetColor Background intensity
                                                   color : withForeground) back
  in
    fromString (setSGRCode withBackground)
switchGraphics Options { consoleIntensity = consIntensity1,
                         swapForegroundBackground = swap1,
                         underlining = underline1,
                         foreground = fore1,
                         background = back1,
                         blinkSpeed = blink1 }
               Options { consoleIntensity = consIntensity2,
                         swapForegroundBackground = swap2,
                         underlining = underline2,
                         foreground = fore2,
                         background = back2,
                         blinkSpeed = blink2 } =
  let
    withConsIntensity =
      if consIntensity1 /= consIntensity2
        then maybe [] ((: []) . SetConsoleIntensity) consIntensity2
        else []
    withUnderline =
      if underline1 /= underline2
        then maybe withConsIntensity ((: withConsIntensity) . SetUnderlining)
                   underline2
        else withConsIntensity
    withBlink =
      if blink1 /= blink2
        then maybe withUnderline ((: withUnderline) . SetBlinkSpeed) blink2
        else withUnderline
    withSwap =
      if swap1 /= swap2
        then maybe withBlink ((: withBlink) . SetSwapForegroundBackground) swap2
        else withBlink
    withForeground =
      if fore1 /= fore2
        then maybe withSwap (\(color, intensity) ->
                              SetColor Foreground intensity color : withSwap)
                   fore2
        else withSwap
    withBackground =
      if back1 /= back2
        then maybe withSwap (\(color, intensity) ->
                              SetColor Background intensity color :
                              withForeground) back2
        else withForeground
  in
    fromString (setSGRCode withBackground)

-- | An empty 'Doc'.
empty :: Doc
empty = Empty

-- | A 'Doc' consisting of a linebreak, that is not turned into a
-- space when erased by a 'group'.
line :: Doc
line = Line { insertSpace = False }

-- | A 'Doc' consisting of a linebreak, that is turned into a space
-- when erased by a 'group'.
linebreak :: Doc
linebreak = Line { insertSpace = True }

-- | A 'Doc' consisting of a space character, that can be turned into
-- a linebreak in order to break lines that are too long.
softline :: Doc
softline = Choose { chooseOptions = [ char ' ', linebreak ] }

-- | An empty 'Doc' that can be turned into a linebreak in order to
-- break lines that are too long.
softbreak :: Doc
softbreak = Choose { chooseOptions = [ empty, line ] }

-- | A 'Doc' containing a single character.
char :: Char -> Doc
char '\n' = line
char chr = Char { charContent = chr }

-- | Create a 'Doc' containing a string.
string :: String -> Doc
string str =
  let
    len = length str
    bstr = Strict.UTF8.fromString str
  in
    Bytestring { bsLength = len, bsContent = bstr }

-- | Create a 'Doc' containing a bytestring.
bytestring :: Strict.ByteString -> Doc
bytestring txt
  | Strict.null txt = Empty
  | otherwise = Bytestring { bsLength = Strict.UTF8.length txt,
                             bsContent = txt }

-- | Create a 'Doc' containing a lazy bytestring
lazyBytestring :: Lazy.ByteString -> Doc
lazyBytestring txt
  | Lazy.null txt = Empty
  | otherwise = LazyBytestring { lbsLength = Lazy.UTF8.length txt,
                                 lbsContent = txt }

-- | The character @(@
lparen :: Doc
lparen = char '('

-- | The character @)@
rparen :: Doc
rparen = char ')'

-- | The character @[@
lbrack :: Doc
lbrack = char '['

-- | The character @]@
rbrack :: Doc
rbrack = char ']'

-- | The character @{@
lbrace :: Doc
lbrace = char '{'

-- | The character @}@
rbrace :: Doc
rbrace = char '}'

-- | The character @<@
langle :: Doc
langle = char '<'

-- | The character @>@
rangle :: Doc
rangle = char '>'

-- | The character @'@
squote :: Doc
squote = char '\''

-- | The character @"@
dquote :: Doc
dquote = char '"'

-- | The character @`@
backquote :: Doc
backquote = char '`'

-- | The character @,@
comma :: Doc
comma = char ','

-- | The character @;@
semi :: Doc
semi = char ';'

-- | The character @:@
colon :: Doc
colon = char ':'

-- | The character @.@
dot :: Doc
dot = char '.'

-- | The character @\@
backslash :: Doc
backslash = char '\\'

-- | A space character.
space :: Doc
space = char ' '

-- | The character @=@
equals :: Doc
equals = char '='

-- | Increase the indentation level of a document by some amount.
nest :: Int -> Doc -> Doc
nest _ Empty = Empty
nest lvl n @ Nest { nestLevel = lvl' } = n { nestLevel = lvl + lvl' }
nest lvl content = Nest { nestLevel = lvl, nestDoc = content }

-- | Set the indentation level to the current column.
align :: Doc -> Doc
align inner = Align { alignDoc = inner }

-- | Enclose a 'Doc' in single quotes
squoted :: Doc -> Doc
squoted = enclose squote squote

-- | Enclose a 'Doc' in double quotes
dquoted :: Doc -> Doc
dquoted = enclose dquote dquote

-- | Enclose a 'Doc' in paretheses
parens :: Doc -> Doc
parens = enclose lparen rparen

-- | Enclose a 'Doc' in brackets
brackets :: Doc -> Doc
brackets = enclose lbrack rbrack

-- | Enclose a 'Doc' in braces
braces :: Doc -> Doc
braces = enclose lbrace rbrace

-- | Enclose a 'Doc' in angles
angles :: Doc -> Doc
angles = enclose langle rangle

-- | Set the graphics mode on a document.
graphics :: Graphics -> Doc -> Doc
graphics sgr doc = Graphics { graphicsDoc = doc, graphicsSGR = sgr }

-- | Color a 'Doc' dull white.
dullWhite :: Doc -> Doc
dullWhite = graphics Options { consoleIntensity = Nothing,
                               underlining = Nothing,
                               blinkSpeed = Nothing,
                               foreground = Just (White, Dull),
                               background = Nothing,
                               swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull red.
dullRed :: Doc -> Doc
dullRed = graphics Options { consoleIntensity = Nothing,
                             underlining = Nothing,
                             blinkSpeed = Nothing,
                             foreground = Just (Red, Dull),
                             background = Nothing,
                             swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull yellow.
dullYellow :: Doc -> Doc
dullYellow = graphics Options { consoleIntensity = Nothing,
                                underlining = Nothing,
                                blinkSpeed = Nothing,
                                foreground = Just (Yellow, Dull),
                                background = Nothing,
                                swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull green.
dullGreen :: Doc -> Doc
dullGreen = graphics Options { consoleIntensity = Nothing,
                               underlining = Nothing,
                               blinkSpeed = Nothing,
                               foreground = Just (Green, Dull),
                               background = Nothing,
                               swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull blue.
dullBlue :: Doc -> Doc
dullBlue = graphics Options { consoleIntensity = Nothing,
                              underlining = Nothing,
                              blinkSpeed = Nothing,
                              foreground = Just (Blue, Dull),
                              background = Nothing,
                              swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull cyan.
dullCyan :: Doc -> Doc
dullCyan = graphics Options { consoleIntensity = Nothing,
                              underlining = Nothing,
                              blinkSpeed = Nothing,
                              foreground = Just (Cyan, Dull),
                              background = Nothing,
                              swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull magenta.
dullMagenta :: Doc -> Doc
dullMagenta = graphics Options { consoleIntensity = Nothing,
                                 underlining = Nothing,
                                 blinkSpeed = Nothing,
                                 foreground = Just (Magenta, Dull),
                                 background = Nothing,
                                 swapForegroundBackground = Nothing }

-- | Color a 'Doc' dull black.
dullBlack :: Doc -> Doc
dullBlack = graphics Options { consoleIntensity = Nothing,
                               underlining = Nothing,
                               blinkSpeed = Nothing,
                               foreground = Just (Black, Dull),
                               background = Nothing,
                               swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid white.
vividWhite :: Doc -> Doc
vividWhite = graphics Options { consoleIntensity = Nothing,
                                underlining = Nothing,
                                blinkSpeed = Nothing,
                                foreground = Just (White, Vivid),
                                background = Nothing,
                                swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid red.
vividRed :: Doc -> Doc
vividRed = graphics Options { consoleIntensity = Nothing,
                              underlining = Nothing,
                              blinkSpeed = Nothing,
                              foreground = Just (Red, Vivid),
                              background = Nothing,
                              swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid yellow.
vividYellow :: Doc -> Doc
vividYellow = graphics Options { consoleIntensity = Nothing,
                                 underlining = Nothing,
                                 blinkSpeed = Nothing,
                                 foreground = Just (Yellow, Vivid),
                                 background = Nothing,
                                 swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid green.
vividGreen :: Doc -> Doc
vividGreen = graphics Options { consoleIntensity = Nothing,
                                underlining = Nothing,
                                blinkSpeed = Nothing,
                                foreground = Just (Green, Vivid),
                                background = Nothing,
                                swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid blue.
vividBlue :: Doc -> Doc
vividBlue = graphics Options { consoleIntensity = Nothing,
                               underlining = Nothing,
                               blinkSpeed = Nothing,
                               foreground = Just (Blue, Vivid),
                               background = Nothing,
                               swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid cyan.
vividCyan :: Doc -> Doc
vividCyan = graphics Options { consoleIntensity = Nothing,
                               underlining = Nothing,
                               blinkSpeed = Nothing,
                               foreground = Just (Cyan, Vivid),
                               background = Nothing,
                               swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid magenta.
vividMagenta :: Doc -> Doc
vividMagenta = graphics Options { consoleIntensity = Nothing,
                                  underlining = Nothing,
                                  blinkSpeed = Nothing,
                                  foreground = Just (Magenta, Vivid),
                                  background = Nothing,
                                  swapForegroundBackground = Nothing }

-- | Color a 'Doc' vivid black.
vividBlack :: Doc -> Doc
vividBlack = graphics Options { consoleIntensity = Nothing,
                                underlining = Nothing,
                                blinkSpeed = Nothing,
                                foreground = Just (Black, Vivid),
                                background = Nothing,
                                swapForegroundBackground = Nothing }

-- | Join two 'Doc's with no space in between.
(<>) :: Doc -> Doc -> Doc
(<>) = beside

-- | Join two 'Doc's with a space in between them.
(<+>) :: Doc -> Doc -> Doc
left <+> right = left <> space <> right

-- | Join two 'Doc's with a 'line' in between them.
(<$>) :: Doc -> Doc -> Doc
left <$> right = left <> line <> right

-- | Join two 'Doc's with a 'linebreak' in between them.
(<$$>) :: Doc -> Doc -> Doc
left <$$> right = left <> linebreak <> right

-- | Join two 'Doc's with a 'softline' in between them.
(</>) :: Doc -> Doc -> Doc
left </> right = left <> softline <> right

-- | Join two 'Doc's with a 'softbreak' in between them.
(<//>) :: Doc -> Doc -> Doc
left <//> right = left <> softbreak <> right

-- | Joun 'Doc's with no space in between them.
beside :: Doc -> Doc -> Doc
beside Empty doc = doc
beside doc Empty = doc
beside Cat { catDocs = left } Cat { catDocs = right } =
  Cat { catDocs = left ++ right }
beside left Cat { catDocs = right } = Cat { catDocs = left : right }
beside Cat { catDocs = left } right = Cat { catDocs = left ++ [right] }
beside left right = Cat { catDocs = [left, right] }

-- | Concatenate a list of 'Doc's.  This is generally more efficient
-- than repeatedly using 'beside' or '<>'.
concat :: [Doc] -> Doc
concat [] = Empty
concat docs = Cat { catDocs = docs }

-- | A choice of several options.  Only one of these will be chosen
-- and used to render the final document.
choose :: [Doc] -> Doc
choose [] = Empty
choose [doc] = doc
choose docs = Choose { chooseOptions = docs }

-- | Concatenate a list of 'Doc's.  This is generally more efficient
-- than repeatedly using 'beside' or '<>'.
hcat :: [Doc] -> Doc
hcat docs = Cat { catDocs = docs }

-- | Join a list of 'Doc's with spaces in between each.  This is
-- generally more efficient than repeatedly using '<+>'.
hsep :: [Doc] -> Doc
hsep = mconcat . intersperse space

-- | Join a list of 'Doc's with 'line's in between each.  This is
-- generally more efficient than repeatedly using '<$$>'.
vsep :: [Doc] -> Doc
vsep = mconcat . intersperse line

-- | Join a list of 'Doc's with 'linebreak's in between each.  This is
-- generally more efficient than repeatedly using '<$>'.
vcat :: [Doc] -> Doc
vcat = mconcat . intersperse linebreak

-- | Join a list of 'Doc's using either 'hsep' or 'vsep'.
sep :: [Doc] -> Doc
sep docs = Choose { chooseOptions = [hsep docs, vsep docs] }

-- | Join a list of 'Doc's using either 'hcat' or 'vcat'.
cat :: [Doc] -> Doc
cat docs = Choose { chooseOptions = [hcat docs, vcat docs] }

-- | Join a list of 'Doc's with 'softline's in between each.  This is
-- generally more efficient than repeatedly using '</>'.
fillSep :: [Doc] -> Doc
fillSep = mconcat . intersperse softline

-- | Join a list of 'Doc's with 'softline's in between each.  This is
-- generally more efficient than repeatedly using '<//>'.
fillCat :: [Doc] -> Doc
fillCat = mconcat . intersperse softbreak

-- | Enclose a 'Doc' within two other 'Doc's
enclose :: Doc -> Doc -> Doc -> Doc
enclose left right middle = hcat [left, middle, right]

-- | Concatenate a list of 'Doc's into a single doc, with each element
-- separated from the others by a given 'Doc' representing
-- punctuation.
punctuate :: Doc -> [Doc] -> Doc
punctuate punc = concat . intersperse punc

-- | Enclose a list of 'Doc's, separated by punctuation, and align
-- nesting of the contents to the end of the left enclosing 'Doc'
encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right _ [] = left <> right
encloseSep left right _ [doc] = left <> doc <> right
encloseSep left right middle docs =
  left <> align (punctuate middle docs) <> right

list :: [Doc] -> Doc
list = group . encloseSep lbrack rbrack (comma <> line)

-- | Erase all linebreaks in a 'Doc' and replace them with either
-- spaces or nothing, depending on the kind of linebreak.
flatten :: Doc -> Doc
flatten Line { insertSpace = True } = Char { charContent = ' ' }
flatten Line { insertSpace = False } = Empty
flatten Cat { catDocs = docs } = Cat { catDocs = map flatten docs }
flatten Choose { chooseOptions = docs } =
  Choose { chooseOptions = map flatten docs }
flatten n @ Nest { nestDoc = inner } = n { nestDoc = flatten inner }
flatten Align { alignDoc = inner } = Align { alignDoc = flatten inner }
flatten doc = doc

-- | A 'Doc' that 'choose's between the unmodified argument, or the
-- 'flatten'ed version of the argument.
group :: Doc -> Doc
group doc = Choose { chooseOptions = [ doc, flatten doc ] }

buildOneLine :: Doc -> Builder
buildOneLine Empty = mempty
buildOneLine Char { charContent = chr } = fromChar chr
buildOneLine Bytestring { bsContent = txt } = fromByteString txt
buildOneLine LazyBytestring { lbsContent = txt } = fromLazyByteString txt
buildOneLine Line { insertSpace = True } = fromChar ' '
buildOneLine Line { insertSpace = False } = mempty
buildOneLine Cat { catDocs = docs } = mconcat (map buildOneLine docs)
buildOneLine Nest { nestDoc = inner } = buildOneLine inner
buildOneLine Choose { chooseOptions = first : _ } = buildOneLine first
buildOneLine Choose {} = error "Choose with no options"
buildOneLine Align { alignDoc = inner } = buildOneLine inner
buildOneLine Graphics { graphicsDoc = inner } = buildOneLine inner

-- | Render the entire document to one line.  Good for output that
-- will be read only by a machine, where newlines are not important at all
renderOneLine :: Doc -> Lazy.ByteString
renderOneLine = toLazyByteString . buildOneLine

buildFast :: Doc -> Builder
buildFast Empty = mempty
buildFast Char { charContent = chr } = fromChar chr
buildFast Bytestring { bsContent = txt } = fromByteString txt
buildFast LazyBytestring { lbsContent = txt } = fromLazyByteString txt
buildFast Line {} = fromChar '\n'
buildFast Cat { catDocs = docs } = mconcat (map buildFast docs)
buildFast Nest { nestDoc = inner } = buildFast inner
buildFast Choose { chooseOptions = first : _ } = buildFast first
buildFast Choose {} = error "Choose with no options"
buildFast Align { alignDoc = inner } = buildFast inner
buildFast Graphics { graphicsDoc = inner } = buildFast inner

-- | Render the entire document, preserving newlines, but without any
-- indentation.  Good for output that will be read only by machine,
-- but where newlines matter.
renderFast :: Doc -> Lazy.ByteString
renderFast = toLazyByteString . buildFast

-- | A rendering of a document.
data Render =
  Render {
    -- | The number of lines in the document.
    renderLines :: !Word,
    -- | The largest amount by which we've overrun.
    renderOverrun :: !Column,
    -- | A builder that constructs the document.
    renderBuilder :: Int -> Builder
  }

-- | Column data type.  Represents how rendered documents affect the
-- current column.
data Column =
    -- | An absolute column offset.
    Fixed { fixedOffset :: !Int }
    -- | A relative column offset.
  | Relative { relOffset :: !Int }

instance Hashable Column where
  hashWithSalt s Fixed { fixedOffset = n } =
    s `hashWithSalt` (0 :: Int) `hashWithSalt` n
  hashWithSalt s Relative { relOffset = n } =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` n

instance Ord Column where
  compare Fixed { fixedOffset = n1 } Fixed { fixedOffset = n2 } = compare n1 n2
  compare Fixed { fixedOffset = n1 } Relative { relOffset = n2 } =
    case compare n1 n2 of
      EQ -> LT
      out -> out
  compare Relative { relOffset = n1 } Fixed { fixedOffset = n2 } =
    case compare n1 n2 of
      EQ -> GT
      out -> out
  compare Relative { relOffset = n1 } Relative { relOffset = n2 } =
    compare n1 n2

instance Eq Column where
  c1 == c2 = compare c1 c2 == EQ

-- | Given a starting column and an ending column, give a column
-- representing the combination of the two.
advance :: Column -> Column -> Column
advance _ f @ Fixed {} = f
advance Fixed { fixedOffset = start } Relative { relOffset = n } =
  Fixed { fixedOffset = start + n }
advance Relative { relOffset = start } Relative { relOffset = n } =
  Relative { relOffset = start + n }

data Offsets =
  Offsets {
    offsetUpper :: !Int,
    offsetCol :: !Column
  }
  deriving Eq

instance Hashable Offsets where
  hashWithSalt s Offsets { offsetUpper = upper, offsetCol = col } =
    s `hashWithSalt` upper `hashWithSalt` col

data Result =
    Single {
      -- | The rendered document.
      singleRender :: !Render,
      -- | The first column at which this render causes an overrun.
      singleUpper :: !Int,
      -- | The current column at the end of rendering.
      singleCol :: !Column
    }
  | Multi {
      -- | A multi-level map.  The first map is indexed by the column
      -- upper-bound (meaning the first column at which using any of
      -- the contents will cause an overrun).  The second map is
      -- indexed by the ending column.
      multiOptions :: !(HashMap Offsets Render)
    }

bestRender :: Render -> Render -> Render
bestRender r1 @ Render { renderLines = lines1, renderOverrun = overrun1 }
           r2 @ Render { renderLines = lines2, renderOverrun = overrun2 }
  | overrun1 < overrun2 = r1
  | overrun1 > overrun2 = r2
  | otherwise = if lines1 < lines2 then r1 else r2

insertRender :: Int -> Column -> Render -> HashMap Offsets Render ->
                HashMap Offsets Render
insertRender upper col render =
  let
    offsets = Offsets { offsetUpper = upper, offsetCol = col }
  in
    HashMap.insertWith bestRender offsets render

packResult :: HashMap Offsets Render -> Result
packResult opts =
  case HashMap.toList opts of
    [(Offsets { offsetUpper = upper, offsetCol = col }, render)] ->
      Single { singleCol = col, singleUpper = upper,
               singleRender = render }
    _ -> Multi { multiOptions = opts }

bestRenderInOpts :: HashMap Offsets Render -> Render
bestRenderInOpts =
  let
    -- | Compare two Renders.  Less than means better.
    compareRenders Render { renderLines = lines1, renderOverrun = overrun1 }
                   Render { renderLines = lines2, renderOverrun = overrun2 } =
      case compare overrun1 overrun2 of
        EQ -> compare lines1 lines2
        out -> out
  in
    minimumBy compareRenders . HashMap.elems

appendOne :: (Int, Column, Render) -> (Int, Column, Render) ->
             (Int, Column, Render)
appendOne (upper1, col1, Render { renderBuilder = build1,
                                  renderLines = lines1,
                                  renderOverrun = overrun1 })
          (upper2, col2, Render { renderBuilder = build2,
                                  renderLines = lines2,
                                  renderOverrun = overrun2 }) =
  let
    (newupper, newbuild) = case col1 of
      Fixed { fixedOffset = n } ->
        (upper1, \col -> build1 col `mappend` build2 n)
      Relative { relOffset = n } ->
        (min upper1 (upper2 - n),
         \col -> build1 col `mappend` (build2 $! col + n))

    newoverrun =
      if newupper < 0
        then Relative { relOffset = abs newupper }
        else Fixed { fixedOffset = 0 }
  in
    (newupper, col1 `advance` col2,
     Render { renderBuilder = newbuild,
              renderOverrun = max (max overrun1 overrun2) newoverrun,
              renderLines = lines1 + lines2 })

appendResults :: Result -> Result -> Result
appendResults Single { singleUpper = upper1, singleCol = col1,
                       singleRender = render1 }
              Single { singleUpper = upper2, singleCol = col2,
                       singleRender = render2 } =
  let
    (newupper, newcol, newrender) =
      appendOne (upper1, col1, render1) (upper2, col2, render2)
  in
    Single { singleUpper = newupper, singleCol = newcol,
             singleRender = newrender }
appendResults Single { singleCol = col1, singleUpper = upper1,
                       singleRender = render1 }
              Multi { multiOptions = opts } =
  let
    foldfun :: HashMap Offsets Render -> Offsets -> Render ->
               HashMap Offsets Render
    foldfun accum Offsets { offsetUpper = upper2, offsetCol = col2 } render2 =
      let
         (newupper, newcol, newrender) = appendOne (upper1, col1, render1)
                                                   (upper2, col2, render2)
      in
        insertRender newupper newcol newrender accum
  in
    packResult (HashMap.foldlWithKey' foldfun HashMap.empty opts)
appendResults Multi { multiOptions = opts }
              Single { singleUpper = upper2, singleCol = col2,
                       singleRender = render2 } =
  let
    foldfun :: HashMap Offsets Render -> Offsets -> Render ->
               HashMap Offsets Render
    foldfun accum Offsets { offsetUpper = upper1, offsetCol = col1 } render1 =
      let
         (newupper, newcol, newrender) = appendOne (upper1, col1, render1)
                                                   (upper2, col2, render2)
      in
        insertRender newupper newcol newrender accum
  in
    packResult (HashMap.foldlWithKey' foldfun HashMap.empty opts)
appendResults Multi { multiOptions = opts1 } Multi { multiOptions = opts2 } =
  let
    outerfold :: HashMap Offsets Render -> Offsets -> Render ->
                 HashMap Offsets Render
    outerfold accum Offsets { offsetUpper = upper1, offsetCol = col1 }
              render1 =
      let
        innerfold :: HashMap Offsets Render -> Offsets -> Render ->
                     HashMap Offsets Render
        innerfold accum' Offsets { offsetUpper = upper2, offsetCol = col2 }
                  render2 =
          let
             (newupper, newcol, newrender) = appendOne (upper1, col1, render1)
                                                       (upper2, col2, render2)
          in
            insertRender newupper newcol newrender accum'
      in
        HashMap.foldlWithKey' innerfold accum opts2
  in
    packResult (HashMap.foldlWithKey' outerfold HashMap.empty opts1)

-- | Combine two results into an option
mergeResults :: Result -> Result -> Result
mergeResults s1 @ Single { singleRender = r1 @ Render { renderLines = lines1 },
                           singleUpper = upper1,
                           singleCol = col1 }
             s2 @ Single { singleRender = r2 @ Render { renderLines = lines2 },
                           singleUpper = upper2,
                           singleCol = col2 }
  | upper1 == upper2 && col1 == col2 = if lines1 < lines2 then s1 else s2
  | otherwise =
    Multi { multiOptions = HashMap.fromList [(Offsets { offsetUpper = upper1,
                                                        offsetCol = col1 },
                                              r1),
                                             (Offsets { offsetUpper = upper2,
                                                        offsetCol = col2 },
                                              r2)] }
mergeResults Single { singleRender = render, singleUpper = upper,
                      singleCol = col }
             Multi { multiOptions = opts } =
  let
    offsets = Offsets { offsetUpper = upper, offsetCol = col }
  in
   Multi { multiOptions = HashMap.insertWith bestRender offsets render opts }
mergeResults m @ Multi {} s @ Single {} = mergeResults s m
mergeResults Multi { multiOptions = opts1 } Multi { multiOptions = opts2 } =
  Multi { multiOptions = HashMap.unionWith bestRender opts1 opts2 }

makespaces :: Int -> Builder
makespaces n = fromLazyByteString (Lazy.Char8.replicate (fromIntegral n) ' ')

renderDynamic :: Int
              -- ^ The maximum number of columns.
              -> Bool
              -- ^ Whether or not to render with ANSI terminal options.
              -> Doc
              -- ^ The document to render.
              -> Lazy.ByteString
renderDynamic maxcol ansiterm doc =
  let
    buildDynamic :: Graphics -> Column -> Doc -> Result
    -- The empty document has only one rendering option.
    buildDynamic _ _ Empty =
      Single {
        singleRender = Render { renderOverrun = Fixed { fixedOffset = 0 },
                                renderBuilder = const mempty,
                                renderLines = 0 },
        singleCol = Relative 0,
        singleUpper = maxcol
      }
    -- For char, bytestring, and lazy bytestring,
    buildDynamic _ _ Char { charContent = chr } =
      let
        overrun = if maxcol >= 1 then Relative 0 else Relative (maxcol - 1)
      in
        Single {
          singleRender = Render { renderLines = 0, renderOverrun = overrun,
                                  renderBuilder = const (fromChar chr) },
          singleCol = Relative 1,
          singleUpper = maxcol - 1
        }
    buildDynamic _ _ Bytestring { bsContent = txt, bsLength = len } =
      let
        overrun = if maxcol >= len then Relative 0 else Relative (len - maxcol)
      in
        Single {
          singleRender = Render { renderBuilder = const (fromByteString txt),
                                  renderOverrun = overrun, renderLines = 0 },
          singleCol = Relative len,
          singleUpper = maxcol - len
        }
    buildDynamic _ _ LazyBytestring { lbsContent = txt, lbsLength = len } =
      let
        overrun = if maxcol >= len then Relative 0 else Relative (len - maxcol)
      in
        Single {
          singleRender =
             Render { renderBuilder = const (fromLazyByteString txt),
                      renderOverrun = overrun, renderLines = 0 },
          singleCol = Relative len,
          singleUpper = maxcol - len
        }
    buildDynamic _ nesting Line {} =
      let
        builder =
          case nesting of
            Relative { relOffset = n } ->
              \len -> fromChar '\n' `mappend` makespaces (len + n)
            Fixed { fixedOffset = n } ->
              const (fromChar '\n' `mappend` makespaces n)
      in
        Single {
          singleRender = Render { renderOverrun = Fixed { fixedOffset = 0 },
                                  renderBuilder = builder,
                                  renderLines = 1 },
          singleCol = nesting,
          singleUpper = maxcol
        }
    buildDynamic _ _ Cat { catDocs = [] } =
      Single {
        singleRender = Render { renderOverrun = Fixed { fixedOffset = 0 },
                                renderBuilder = const mempty,
                                renderLines = 0 },
        singleCol = Relative { relOffset = 0 },
        singleUpper = maxcol
      }
    buildDynamic sgr nesting Cat { catDocs = first : rest } =
      let
        firstres = buildDynamic sgr nesting first
        restres = map (buildDynamic sgr nesting) rest
      in
        foldl appendResults firstres restres
    buildDynamic sgr nesting Nest { nestLevel = inc, nestDoc = inner } =
      let
        newnesting = case nesting of
          Fixed { fixedOffset = n } -> Fixed { fixedOffset = n + inc }
          Relative { relOffset = n } -> Relative { relOffset = n + inc }
      in
       buildDynamic sgr newnesting inner
    buildDynamic sgr nesting Choose { chooseOptions = options } =
      let
        results = map (buildDynamic sgr nesting) options
      in
        foldl1 mergeResults results
    buildDynamic sgr _ Align { alignDoc = inner } =
      buildDynamic sgr (Relative 0) inner
    buildDynamic sgr1 nesting Graphics { graphicsSGR = sgr2,
                                         graphicsDoc = inner }
      | ansiterm =
        let
          wrapBuilder r @ Render { renderBuilder = build } =
            r { renderBuilder = \col -> switchGraphics sgr1 sgr2 `mappend`
                                        build col `mappend`
                                        switchGraphics sgr2 sgr1 }
        in case buildDynamic sgr2 nesting inner of
          s @ Single { singleRender = render } ->
            s { singleRender = wrapBuilder render }
          m @ Multi { multiOptions = opts } ->
            m { multiOptions = HashMap.map wrapBuilder opts }
      | otherwise = buildDynamic sgr2 nesting inner

    Render { renderBuilder = result } =
      case buildDynamic Default Fixed { fixedOffset = 0 } doc of
        Single { singleRender = render } -> render
        Multi opts -> bestRenderInOpts opts
  in
    toLazyByteString (result 0)

instance Monoid Doc where
  mempty = Empty
  mappend = beside

-- | A class representing datatypes that can be formatted as 'Doc's.
class Format item where
  -- | Format an @item@ as a 'Doc'
  format :: item -> Doc

-- | Format a list of @item@s as a 'Doc'
  formatList :: [item] -> Doc
  formatList = list . map format

instance Format a => Format [a] where
  format = formatList

instance Format Doc where
  format = id

instance Format String where
  format = string

instance Format Strict.ByteString where
  format = bytestring

instance Format Lazy.ByteString where
  format = lazyBytestring

instance Format Int where
  format = string . show

instance Format Integer where
  format = string . show

instance Format Word where
  format = string . show

instance Format Float where
  format = string . show

instance Format Double where
  format = string . show
