{
{-# OPTIONS_GHC
  -fno-warn-missing-deriving-strategies
  -fno-warn-missing-exported-signatures
  -fno-warn-missing-import-lists
  -fno-warn-missing-local-signatures
  -fno-warn-monomorphism-restriction
  -fno-warn-unused-imports
  -funbox-strict-fields
#-}
module Language.Tiger.Lexer
  ( Token (..)
  , Alex, AlexPosn (..)
  , alexError
  , alexGetInput
  , alexMonadScan
  , runAlex
  ) where

import Prelude hiding (Ordering (..))

import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Char (chr, ord)
import Data.Ix (inRange)
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = $alpha ($alpha | $digit | \_)*

tokens :-

<0> $white+ ;

<0> type     { tok Type }
<0> var      { tok Var }
<0> function { tok Function }
<0> break    { tok Break }
<0> of       { tok Of }
<0> end      { tok End }
<0> in       { tok In }
<0> nil      { tok Nil }
<0> let      { tok Let }
<0> do       { tok Do }
<0> to       { tok To }
<0> for      { tok For }
<0> while    { tok While }
<0> else     { tok Else }
<0> then     { tok Then }
<0> if       { tok If }
<0> array    { tok Array }
<0> ":="     { tok Assign }
<0> "|"      { tok Or }
<0> "&"      { tok And }
<0> ">="     { tok GE }
<0> ">"      { tok GT }
<0> "<="     { tok LE }
<0> "<"      { tok LT }
<0> "<>"     { tok NEQ }
<0> "="      { tok EQ }
<0> "/"      { tok Divide }
<0> "*"      { tok Times }
<0> "-"      { tok Minus }
<0> "+"      { tok Plus }
<0> "."      { tok Dot }
<0> "}"      { tok RBrace }
<0> "{"      { tok LBrace }
<0> "]"      { tok RBrack }
<0> "["      { tok LBrack }
<0> ")"      { tok RParen }
<0> "("      { tok LParen }
<0> ";"      { tok Semicolon }
<0> ":"      { tok Colon }
<0> ","      { tok Comma }
<0> $digit+  { \(p, _, s, _) len -> pure $ Int p $ read $ BS.unpack $ BS.take len s }
<0> @id      { \(p, _, s, _) len -> pure $ Id p $ BS.take len s }

<0> "/*"       { nestComment `andBegin` comment }
<comment> "/*" { nestComment }
<comment> "*/" { unnestComment }
<comment> .    ;
<comment> \n   ;

<0> \"               { enterString `andBegin` string }
<string> \"          { exitString }
<string> \\n         { emit '\n' }
<string> \\t         { emit '\t' }
<string> \\\^.       { emitControl }
<string> \\$digit{3} { emitASCII }
<string> \\\"        { emit '"' } -- "}
<string> \\\\        { emit '\\' }
<string> \\$white+\\ ;
<string> .           { emitCurrent }

{
data AlexUserState = AlexUserState
  { lexerCommentDepth :: Int
  , lexerCurrentString :: String
  , lexerInString :: Bool
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { lexerCommentDepth = 0
  , lexerCurrentString = ""
  , lexerInString = False
  }

get :: Alex AlexUserState
get = Alex \s' -> let s = alex_ust s' in Right (s', s)

put :: AlexUserState -> Alex ()
put s = Alex \s' -> Right (s'{alex_ust = s}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex \s' -> Right (s'{alex_ust = f (alex_ust s')}, ())

nestComment, unnestComment :: AlexAction Token
nestComment input len = do
  incrementCommentDepth
  skip input len
unnestComment input len = do
  decrementCommentDepth
  AlexUserState{lexerCommentDepth} <- get
  when (lexerCommentDepth == 0) (alexSetStartCode 0)
  skip input len

incrementCommentDepth, decrementCommentDepth :: Alex ()
incrementCommentDepth = modify \s -> s{lexerCommentDepth = lexerCommentDepth s + 1}
decrementCommentDepth = modify \s -> s{lexerCommentDepth = lexerCommentDepth s - 1}

insertChar :: Char -> Alex ()
insertChar c = modify \s -> s{lexerCurrentString = c : lexerCurrentString s}

emit :: Char -> AlexAction Token
emit c _ _ = do
  insertChar c
  alexMonadScan

formatPos :: AlexPosn -> String
formatPos (AlexPn _ l c) = show l <> ":" <> show c

panic :: AlexAction a
panic (p, _, str, _) len =
  alexError $
    "panic! Function was called unexpectedly with `" <> BS.unpack (BS.take len str) <> "`,"
    <> " at " <> formatPos p <> "."

userLexerError :: String -> AlexAction a
userLexerError msg (p, _, str, _) len =
  alexError $
    "Lexical error in `" <> BS.unpack (BS.take len str) <> "`,"
    <> " at " <> formatPos p <> ".\n"
    <> msg

emitControl :: AlexAction Token
emitControl inp@(_, _, str, _) len
  -- Backslash followed by caret and a character: \^c
  | len == 3  = either userLexerError emit code inp len
  | otherwise = panic inp len
  where
    x = BS.index str 2
    -- Reference: https://en.wikipedia.org/wiki/C0_and_C1_control_codes
    code
      | inRange ('@', '_') x = Right $ chr (ord x - ord '@')
      | otherwise            = Left $ "Invalid C0 control code: " <> [x]

emitASCII :: AlexAction Token
emitASCII inp@(_, _, str, _) len
  -- Backslash followed by three digits: \abc
  | len == 4  = either userLexerError emit code inp len
  | otherwise = panic inp len
  where
    c1 = BS.index str 1
    c2 = BS.index str 2
    c3 = BS.index str 3
    zero = ord '0'
    x = ord c1 - zero
    y = ord c2 - zero
    z = ord c3 - zero
    code
      | x <  2                     = Right $ chr (100 * x + 10 * y + z)
      | x == 2 && y <  5           = Right $ chr (200 + 10 * y + z)
      | x == 2 && y == 5 && z <= 5 = Right $ chr (250 + z)
      | otherwise                  = Left $ "Invalid number (> 255): " <> [c1, c2, c3]

emitCurrent :: AlexAction Token
emitCurrent inp@(_, _, str, _) len
  | len == 1  = emit (BS.head str) inp len
  | otherwise = panic inp len

enterString :: AlexAction Token
enterString _ _ = do
  modify \s -> s{lexerInString = True}
  alexMonadScan

exitString :: AlexAction Token
exitString (p, _, _, _) _ = do
  s <- get
  let str = lexerCurrentString s
  put s{lexerCurrentString = "", lexerInString = False}
  alexSetStartCode 0
  pure $ String p $ BS.pack $ reverse str

tok :: (AlexPosn -> token) -> AlexAction token
tok ctor (p, _, _, _) _ = pure $ ctor p

data Token
  = Type AlexPosn
  | Var AlexPosn
  | Function AlexPosn
  | Break AlexPosn
  | Of AlexPosn
  | End AlexPosn
  | In AlexPosn
  | Nil AlexPosn
  | Let AlexPosn
  | Do AlexPosn
  | To AlexPosn
  | For AlexPosn
  | While AlexPosn
  | Else AlexPosn
  | Then AlexPosn
  | If AlexPosn
  | Array AlexPosn
  | Assign AlexPosn
  | Or AlexPosn
  | And AlexPosn
  | GE AlexPosn
  | GT AlexPosn
  | LE AlexPosn
  | LT AlexPosn
  | NEQ AlexPosn
  | EQ AlexPosn
  | Divide AlexPosn
  | Times AlexPosn
  | Minus AlexPosn
  | Plus AlexPosn
  | Dot AlexPosn
  | RBrace AlexPosn
  | LBrace AlexPosn
  | RBrack AlexPosn
  | LBrack AlexPosn
  | RParen AlexPosn
  | LParen AlexPosn
  | Semicolon AlexPosn
  | Colon AlexPosn
  | Comma AlexPosn
  | String AlexPosn ByteString
  | Int AlexPosn Integer
  | Id AlexPosn ByteString
  | EOF AlexPosn
  deriving stock (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
  s <- get
  if
    | lexerInString s -> alexError "Unclosed string at end of file."
    | lexerCommentDepth s > 0 -> alexError "Unclosed comment at end of file."
    | otherwise -> do
      (pos, _, _, _) <- alexGetInput
      pure $ EOF pos
}
