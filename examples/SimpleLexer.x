{
{-# OPTIONS_GHC -funbox-strict-fields #-}

module SimpleLexer where

import Control.Monad.Lexer
import Control.Monad.Keywords
import Control.Monad.Messages
import Control.Monad.SourceBuffer
import Control.Monad.Symbols
import Control.Monad.Trans
import Data.LexicalError
import Data.Message
import Data.Position
import Data.Symbol
import System.IO

import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Char8 as Strict

import Text.AlexWrapper

}

tokens :-

\n                   { \startpos endpos @ (AlexPn off _ _) tokstr ->
                          do
			    linebreak off
			    skip startpos endpos tokstr
                     }
[\ \t\r\f]+          { skip }
[\+\-\*\/]+          { token (keywordOrToken Operator) }
[0-9]+               { token (\bstr -> return . (Number (read (Lazy.unpack bstr)))) }
[a-zA-Z][a-zA-Z0-9]* { token (keywordOrToken Name) }

{

-- Define a list of keywords.  These will be passed into the run
-- function for the Keywords monad.

keywords = [(Strict.pack "+", Plus),
            (Strict.pack "-", Minus),
            (Strict.pack "*", Times),
            (Strict.pack "/", Div),
            (Strict.pack "mod", Mod)]

-- Tokens and messages would usually be defined in another module.

data Token =
    Plus Position
  | Minus Position
  | Times Position
  | Div Position
  | Mod Position
  | Name Symbol Position
  | Number Integer Position
  | Operator Symbol Position
  | EOF

data LexerMessage =
  BadChars Lazy.ByteString Position

instance Message LexerMessage where
  severity _ = Error
  position (BadChars _ pos) = Just pos
  kind (BadChars _ _) = Strict.pack "BadChars"
  brief (BadChars _ _) = Lazy.pack "Invalid characters in input"
  details (BadChars _ _) = Lazy.empty

instance LexicalError LexerMessage where
  mkInvalidChars = BadChars

type SimpleLexer =
  AlexT () (MessagesT [LexerMessage] LexerMessage (Lexer Token))

putToken :: Token -> SimpleLexer ()
putToken (Plus pos) =
  do
    pinfo <- positionInfo pos
    liftIO (putStrLn $! "plus at " ++ show pinfo)
putToken (Minus pos) =
  do
    pinfo <- positionInfo pos
    liftIO (putStrLn $! "minus at " ++ show pinfo)
putToken (Times pos) =
  do
    pinfo <- positionInfo pos
    liftIO (putStrLn $! "times at " ++ show pinfo)
putToken (Div pos) =
  do
    pinfo <- positionInfo pos
    liftIO (putStrLn $! "div at " ++ show pinfo)
putToken (Mod pos) =
  do
    pinfo <- positionInfo pos
    liftIO (putStrLn $! "mod at " ++ show pinfo)
putToken (Name sym pos) =
  do
    pinfo <- positionInfo pos
    namestr <- name sym
    liftIO (putStrLn $! "name " ++ show namestr ++ " at " ++ show pinfo)
putToken (Number n pos) =
  do
    pinfo <- positionInfo pos
    liftIO (putStrLn $! "number " ++ show n ++ " at " ++ show pinfo)
putToken (Operator sym pos) =
  do
    pinfo <- positionInfo pos
    namestr <- name sym
    liftIO (putStrLn $! "operator " ++ show namestr ++ " at " ++ show pinfo)

-- Create a very simple main that reads in input and outputs the
-- tokens that are lexed.

printTokens :: SimpleLexer ()
printTokens =
  do
    tok <- alexMonadScan
    case tok of
      EOF -> return ()
      _ ->
        do
          putToken tok
          printTokens

run :: Lazy.ByteString -> IO ()
run input =
  let
    stdinName = (Strict.pack "stdin")

    run' =
      do
        startFile stdinName input
        printTokens
        finishFile

    messages = runAlexT run' input stdinName ()
    lexerm = putMessagesT stderr Error messages
  in do
    runLexerT lexerm keywords
    return ()

-- Everything from here down is boilerplate.  With AlexHelper, we can
-- minimize the boilerplate quite a bit.
type AlexUserState = ()

type AlexState = AlexInternalState AlexUserState

type AlexAction = AlexMonadAction SimpleLexer Token

scanWrapper :: AlexResultHandlers SimpleLexer Token ->
               AlexInput -> Int -> SimpleLexer Token
scanWrapper handlers inp sc =
  case alexScan inp sc of
    AlexEOF -> handleEOF handlers
    AlexError inp' -> handleError handlers inp'
    AlexSkip inp' len -> handleSkip handlers inp' len
    AlexToken inp' len action -> handleToken handlers inp' len action

alexEOF :: SimpleLexer Token
alexEOF = return EOF

alexMonadScan :: SimpleLexer Token

skip :: AlexAction

begin :: Int -> AlexAction

AlexActions { actAlexMonadScan = alexMonadScan, actSkip = skip,
              actBegin = begin } =
  mkAlexActions scanWrapper invalidChars alexEOF

}
