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

module Tests.Text.Format(tests) where

import Test.HUnitPlus.Base
import Text.Format

import qualified Data.ByteString.Char8 as Strict
import qualified Data.ByteString.UTF8 as Strict
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Lazy.UTF8 as Lazy

renderDynamicTests :: [Test]
renderDynamicTests = [
    "empty" ~: Lazy.empty @=? renderDynamic 1 False empty,
    "char" ~: Lazy.singleton 'a' @=? renderDynamic 1 False (char 'a'),
    "string" ~: Lazy.fromString "hello" @=?
      renderDynamic 1 False (string "hello"),
    "bytestring" ~: Lazy.fromString "hello" @=?
      renderDynamic 1 False (bytestring (Strict.fromString "hello")),
    "lazyBytestring" ~: Lazy.fromString "hello" @=?
      renderDynamic 1 False (lazyBytestring (Lazy.fromString "hello")),
    "line" ~: Lazy.singleton '\n' @?= renderDynamic 1 False line,
    "cat" ~: Lazy.fromString "helloworld" @=?
      renderDynamic 1 False (string "hello" <> string "world"),
    "nest" ~: Lazy.fromString "hello\n  world" @=?
      renderDynamic 1 False (string "hello" <> nest 2 (line <> string "world")),
    "align" ~: Lazy.fromString "hello\n     world" @=?
      renderDynamic 1 False (string "hello" <> align (line <> string "world")),
    "align2" ~: Lazy.fromString "hello\n     world\n     today" @=?
      renderDynamic 1 False (string "hello" <>
                             align (line <> string "world" <>
                                    line <> string "today")),
    "align3" ~: Lazy.fromString "hello\n     world\n          today" @=?
      renderDynamic 1 False (string "hello" <>
                             align (line <> string "world" <>
                                    align (line <> string "today"))),
    "choose_lines" ~: Lazy.fromString "\n" @=?
      renderDynamic 2 False (choose [line <> line <> line, line, line <> line]),
    "choose_overrun" ~: Lazy.fromString " " @=?
      renderDynamic 1 False (choose [string " ", string "  "]),
    "choose_line_cat" ~: Lazy.fromString "hello\nworld" @=?
      renderDynamic 8 False (choose [string "hello ", string "hello" <> line] <>
                       string "world"),
    "choose_nest_line_cat" ~: Lazy.fromString "hello\nworld" @=?
      renderDynamic 8 False (choose [string "hello ",
                               string "hello" <> nest 2 line] <>
                       string "world"),
    "cat_choose" ~: Lazy.fromString "hello\nworld" @=?
      renderDynamic 8 False (string "hello" <>
                             choose [string " world", line <> string "world"]),
    "cat_nest_choose" ~: Lazy.fromString "hello\n  world" @=?
      renderDynamic 8 False (string "hello" <>
                             nest 2 (choose [string " world",
                                             line <> string "world"])),
    "choose_choose" ~: Lazy.fromString "hello you" @=?
      renderDynamic 9 False (choose [string "hello ", string "hello" <> line] <>
                       choose [string "world", string "you"]),
    "softline_break" ~: Lazy.fromString "hello\nworld" @=?
      renderDynamic 6 False (string "hello" <> softline <> string "world"),
    "softline_space" ~: Lazy.fromString "hello world" @=?
      renderDynamic 11 False (string "hello" <> softline <> string "world"),
    "delay_indent" ~: Lazy.fromString "\n\n" @=?
      renderDynamic 2 False (nest 2 (line <> line)),
    "delay_indent2" ~: Lazy.fromString "\n\n  hello" @=?
      renderDynamic 2 False (nest 2 (line <> line <> string "hello")),
    "delay_indent_nest" ~: Lazy.fromString "\n\n    hello" @=?
      renderDynamic 2 False (nest 2 (line <> nest 2 (line <> string "hello"))),
    "align_delay_indent" ~: Lazy.fromString "  \n\n  hello" @=?
      renderDynamic 2 False (string "  " <>
                             align (line <> line <> string "hello")),
    "align_delay_indent_nest" ~: Lazy.fromString "  \n\n    hello" @=?
      renderDynamic 2 False (string "  " <>
                             align (nest 2 (line <> line <> string "hello"))),
    "align_delay_indent_line_nest" ~: Lazy.fromString "  \n\n    hello" @=?
      renderDynamic 2 False (string "  " <>
                             align (line <> nest 2 (line <> string "hello")))
  ]

testlist :: [Test]
testlist = [
    "renderDynamic" ~: renderDynamicTests
  ]

tests :: Test
tests = "Format" ~: testlist
