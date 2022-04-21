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
  ( Token (..), TokenType (..)
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
import Data.Maybe (fromJust)

import Language.Tiger.Position (Pos (..), Range (..))
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
<0> $digit+  { tokDigit }
<0> @id      { tokId }

<0> "/*"       { nestComment `andBegin` comment }
<comment> "/*" { nestComment }
<comment> "*/" { unnestComment }
<comment> .    ;
<comment> \n   ;

<0> \"               { enterString `andBegin` string }
<string> \"          { exitString `andBegin` 0 }
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
  , lexerStringStart :: Maybe Pos
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { lexerCommentDepth = 0
  , lexerCurrentString = ""
  , lexerStringStart = Nothing
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
enterString (p, _, _, _) _ = do
  modify \s -> s{lexerStringStart = Just $ mkPos p}
  alexMonadScan

exitString :: AlexAction Token
exitString (AlexPn offset line column, _, _, _) _ = do
  s <- get
  let str = lexerCurrentString s
  let start = fromJust $ lexerStringStart s
  put s{lexerCurrentString = "", lexerStringStart = Nothing}
  pure $ Token (Range start end) $ String $ BS.pack $ reverse str
  where
    -- XXX: Not sure why, but I had to add one here so the ranges are correct.
    end = Pos
      { line
      , column = column + 1
      , offset = offset + 1
      }

tok :: TokenType -> AlexAction Token
tok ctor inp len = pure $ Token (mkRange inp len) ctor

tokDigit, tokId :: AlexAction Token
tokDigit inp@(_, _, s, _) len =
  pure $ Token (mkRange inp len) $ Int $ read $ BS.unpack $ BS.take len s
tokId inp@(_, _, s, _) len =
  pure $ Token (mkRange inp len) $ Id $ BS.copy $ BS.take len s

mkPos :: AlexPosn -> Pos
mkPos (AlexPn offset line column) = Pos line column offset

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = mkPos start, end = mkPos end}
  where
    end = BS.foldl' alexMove start $ BS.take len str

data Token = Token
  { range :: Range
  , tokenType :: TokenType
  } deriving stock (Eq, Show)

data TokenType
  = Type
  | Var
  | Function
  | Break
  | Of
  | End
  | In
  | Nil
  | Let
  | Do
  | To
  | For
  | While
  | Else
  | Then
  | If
  | Array
  | Assign
  | Or
  | And
  | GE
  | GT
  | LE
  | LT
  | NEQ
  | EQ
  | Divide
  | Times
  | Minus
  | Plus
  | Dot
  | RBrace
  | LBrace
  | RBrack
  | LBrack
  | RParen
  | LParen
  | Semicolon
  | Colon
  | Comma
  | String ByteString
  | Int Integer
  | Id ByteString
  | EOF
  deriving stock (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
  s <- get
  if
    | Just _ <- lexerStringStart s -> alexError "Unclosed string at end of file."
    | lexerCommentDepth s > 0 -> alexError "Unclosed comment at end of file."
    | otherwise -> do
      (AlexPn offset line column, _, _, _) <- alexGetInput
      let pos = Pos line column offset
      pure $ Token (Range pos pos) EOF
}
