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

module Text.Format(
       -- * Basic Definitions
       -- ** Types
       Doc,
       -- ** Type Classes
       Format(..),

       -- * Creating @Doc@s

       -- ** Constructors

       -- *** Basic
       empty,
       char,
       bytestring,
       lazyBytestring,
       line,
       linebreak,
       softline,

       -- *** From datatypes
       string,

       -- *** Literals
       lparen,
       rparen,
       lbrack,
       rbrack,
       lbrace,
       rbrace,
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

       -- ** Combining @Doc@s
       (<>),
       (<+>),
       (<$>),
       (<$$>),
       (</>),
       (<//>),
       beside,
       concat,
       hsep,
       hcat,
       vsep,
       vcat,
       sep,
       cat,
       fillSep,
       fillCat,
       punctuate,

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
import Data.Monoid hiding ((<>))
import Data.Word
import Prelude hiding (concat)

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

-- | An empty 'Doc'.
empty :: Doc
empty = Empty

-- | A 'Doc' containing a single character.
char :: Char -> Doc
char '\n' = line
char chr = Char { charContent = chr }

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

-- | Create a 'Doc' containing a string.
string :: String -> Doc
string str =
  let
    len = length str
    bstr = Strict.UTF8.fromString str
  in
    Bytestring { bsLength = len, bsContent = bstr }

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

-- | A 'Doc' consisting of a linebreak, that is not turned into a
-- space when erased by a 'group'.
line :: Doc
line = Line { insertSpace = False }

-- | A 'Doc' consisting of a linebreak, that is turned into a space
-- when erased by a 'group'.
linebreak :: Doc
linebreak = Line { insertSpace = True }

softline :: Doc
softline = Choose { chooseOptions = [ char ' ', linebreak ] }

softbreak :: Doc
softbreak = Choose { chooseOptions = [ empty, line ] }

-- | Concatenate two 'Doc's with no space in between.
beside :: Doc -> Doc -> Doc
beside Empty doc = doc
beside doc Empty = doc
beside Cat { catDocs = left } Cat { catDocs = right } =
  Cat { catDocs = left ++ right }
beside left Cat { catDocs = right } = Cat { catDocs = left : right }
beside Cat { catDocs = left } right = Cat { catDocs = left ++ [right] }
beside left right = Cat { catDocs = [left, right] }

-- | Concatenate two 'Doc's with no space in between.
(<>) :: Doc -> Doc -> Doc
(<>) = beside

(<$>) :: Doc -> Doc -> Doc
left <$> right = left <> line <> right

(<$$>) :: Doc -> Doc -> Doc
left <$$> right = left <> linebreak <> right

(</>) :: Doc -> Doc -> Doc
left </> right = left <> softline <> right

(<//>) :: Doc -> Doc -> Doc
left <//> right = left <> softbreak <> right

-- | Concatenate two 'Doc's with a space in between.
(<+>) :: Doc -> Doc -> Doc
left <+> right = left <> space <> right

-- | Concatenate a list of 'Doc's on the same line, separated by spaces
hsep :: [Doc] -> Doc
hsep = mconcat . intersperse space

hcat :: [Doc] -> Doc
hcat docs = Cat { catDocs = docs }

vsep :: [Doc] -> Doc
vsep = mconcat . intersperse line

vcat :: [Doc] -> Doc
vcat = mconcat . intersperse linebreak

sep :: [Doc] -> Doc
sep docs = Choose { chooseOptions = [hsep docs, vsep docs] }

cat :: [Doc] -> Doc
cat docs = Choose { chooseOptions = [hcat docs, vcat docs] }

fillSep :: [Doc] -> Doc
fillSep = mconcat . intersperse softline

fillCat :: [Doc] -> Doc
fillCat = mconcat . intersperse softbreak

-- | Concatenate a list of 'Doc's into a single doc, with each element
-- separated from the others by a given 'Doc' representing
-- punctuation.
punctuate :: Doc -> [Doc] -> Doc
punctuate _ [] = Empty
punctuate _ [doc] = doc
punctuate punc docs = Cat { catDocs = intersperse punc docs }

-- | Concatenate a list of 'Doc's.  This is generally more efficient
-- than repeatedly using 'beside'.
concat :: [Doc] -> Doc
concat [] = Empty
concat docs = Cat { catDocs = docs }

-- | Increase the indentation level of a document by some amount.
nest :: Int -> Doc -> Doc
nest _ Empty = Empty
nest lvl n @ Nest { nestLevel = lvl' } = n { nestLevel = lvl + lvl' }
nest lvl content = Nest { nestLevel = lvl, nestDoc = content }

-- | Set the indentation level to the current column.
align :: Doc -> Doc
align inner = Align { alignDoc = inner }

group :: Doc -> Doc
group doc = Choose { chooseOptions = [ doc, flatten doc ] }

flatten :: Doc -> Doc
flatten Line { insertSpace = True } = Char { charContent = ' ' }
flatten Line { insertSpace = False } = Empty
flatten Cat { catDocs = docs } = Cat { catDocs = map flatten docs }
flatten Choose { chooseOptions = docs } =
  Choose { chooseOptions = map flatten docs }
flatten n @ Nest { nestDoc = inner } = n { nestDoc = flatten inner }
flatten Align { alignDoc = inner } = Align { alignDoc = flatten inner }
flatten doc = doc

squoted :: Doc -> Doc
squoted doc = squote <> doc <> squote

dquoted :: Doc -> Doc
dquoted doc = dquote <> doc <> dquote

encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right _ [] = left <> right
encloseSep left right _ [doc] = left <> doc <> right
encloseSep left right middle docs =
  left <> mconcat (intersperse middle docs) <> right

list :: [Doc] -> Doc
list = encloseSep lbrack rbrack comma

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
    Fixed !Int
  | Relative !Int

instance Hashable Column where
  hashWithSalt s (Fixed n) = s `hashWithSalt` (0 :: Int) `hashWithSalt` n
  hashWithSalt s (Relative n) = s `hashWithSalt` (1 :: Int) `hashWithSalt` n

instance Ord Column where
  compare (Fixed n1) (Fixed n2) = compare n1 n2
  compare (Fixed n1) (Relative n2) =
    case compare n1 n2 of
      EQ -> LT
      out -> out
  compare (Relative n1) (Fixed n2) =
    case compare n1 n2 of
      EQ -> GT
      out -> out
  compare (Relative n1) (Relative n2) = compare n1 n2

instance Eq Column where
  c1 == c2 = compare c1 c2 == EQ

-- | Given a starting column and an ending column, return a column
-- that represents advancing to the end.
advance :: Column -> Column -> Column
advance _ f @ (Fixed _) = f
advance (Fixed start) (Relative n) = Fixed (start + n)
advance (Relative start) (Relative n) = Relative (start + n)

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
      -- This reversal is intentional.  Less overrun is better
      case compare overrun2 overrun1 of
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
    newupper = case col1 of
      Relative n -> min upper1 (upper2 - n)
      Fixed _ -> upper1

    newoverrun = if newupper < 0 then Relative (abs newupper) else Fixed 0
  in
    (newupper, col1 `advance` col2,
     Render { renderBuilder = build1 `mappend` build2,
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

renderDynamic :: Int
              -- ^ The maximum number of columns.
              -> Doc
              -- ^ The document to render.
              -> Lazy.ByteString
renderDynamic maxcol doc =
  let

    buildDynamic :: Column -> Doc -> Result
    -- The empty document has only one rendering option.
    buildDynamic _ Empty =
      Single {
        singleRender = Render { renderLines = 0, renderOverrun = Fixed 0,
                                renderBuilder = const mempty },
        singleCol = Relative 0,
        singleUpper = maxcol
      }
    -- For char, bytestring, and lazy bytestring,
    buildDynamic _ Char { charContent = chr } =
      Single {
        singleRender = Render { renderLines = 0, renderOverrun = Fixed 0,
                                renderBuilder = const (fromChar chr) },
        singleCol = Relative 1,
        singleUpper = maxcol - 1
      }
    buildDynamic _ Bytestring { bsContent = txt, bsLength = len } =
      Single {
        singleRender = Render { renderBuilder = const (fromByteString txt),
                                renderOverrun = Fixed 0, renderLines = 0 },
        singleCol = Relative len,
        singleUpper = maxcol - len
      }
    buildDynamic _ LazyBytestring { lbsContent = txt,
                                            lbsLength = len } =
      Single {
        singleRender = Render { renderBuilder = const (fromLazyByteString txt),
                                renderOverrun = Fixed 0, renderLines = 0 },
        singleCol = Relative len,
        singleUpper = maxcol - len
      }
    buildDynamic nesting Line {} =
      let
        makebytestring n =
          fromLazyByteString (Lazy.Char8.replicate (fromIntegral n) ' ')

        builder =
          case nesting of
            Relative n ->
              \len -> fromChar '\n' `mappend` makebytestring (len + n)
            Fixed n -> const (fromChar '\n' `mappend` makebytestring n)
      in
        Single {
          singleRender = Render { renderOverrun = Fixed 0, renderLines = 1,
                                  renderBuilder = builder },
          singleCol = nesting,
          singleUpper = maxcol
        }
    buildDynamic _ Cat { catDocs = [] } =
      Single {
        singleRender = Render { renderLines = 0, renderOverrun = Fixed 0,
                                renderBuilder = const mempty },
        singleCol = Relative 0,
        singleUpper = maxcol
      }
    buildDynamic nesting Cat { catDocs = first : rest } =
      let
        firstres = buildDynamic nesting first
        restres = map (buildDynamic nesting) rest
      in
        foldl appendResults firstres restres
    buildDynamic nesting Nest { nestLevel = inc, nestDoc = inner } =
      let
        newnesting = case nesting of
          Fixed n -> Fixed (n + inc)
          Relative n -> Relative (n + inc)
      in
       buildDynamic newnesting inner
    buildDynamic nesting Choose { chooseOptions = options } =
      let
        results = map (buildDynamic nesting) options
      in
        foldl1 mergeResults results
    buildDynamic _ Align { alignDoc = inner } =
      buildDynamic (Relative 0) inner

    Render { renderBuilder = result } =
      case buildDynamic (Fixed 0) doc of
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
