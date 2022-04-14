{
{-# OPTIONS_GHC
  -fno-warn-missing-deriving-strategies
  -fno-warn-missing-exported-signatures
  -fno-warn-missing-import-lists
  -fno-warn-missing-local-signatures
  -fno-warn-monomorphism-restriction
  -fno-warn-unused-imports
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

import Control.Exception (Exception (..))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
}

%wrapper "monad-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = $alpha ($alpha | $digit | \_)*
-- TODO: Handle escaped characters according to the specification and translate
-- into their meanings
-- TODO: Detect unclosed strings and comments
@string = \" .* \" -- "
@comment = "/*" .* "*/" -- "

tokens :-

$white+ ;
@comment ;

type     { tok Type }
var      { tok Var }
function { tok Function }
break    { tok Break }
of       { tok Of }
end      { tok End }
in       { tok In }
nil      { tok Nil }
let      { tok Let }
do       { tok Do }
to       { tok To }
for      { tok For }
while    { tok While }
else     { tok Else }
then     { tok Then }
if       { tok If }
array    { tok Array }
":="     { tok Assign }
"|"      { tok Or }
"&"      { tok And }
">="     { tok GE }
">"      { tok GT }
"<="     { tok LE }
"<"      { tok LT }
"<>"     { tok NEQ }
"="      { tok EQ }
"/"      { tok Divide }
"*"      { tok Times }
"-"      { tok Minus }
"+"      { tok Plus }
"."      { tok Dot }
"}"      { tok RBrace }
"{"      { tok LBrace }
"]"      { tok RBrack }
"["      { tok LBrack }
")"      { tok RParen }
"("      { tok LParen }
";"      { tok Semicolon }
":"      { tok Colon }
","      { tok Comma }
$digit+  { \(p, _, s, _) len -> pure $ Int p $ read $ BS.unpack $ BS.take len s }
@id      { \(p, _, s, _) len -> pure $ Id p $ BS.take len s }
@string  { \(p, _, s, _) len -> pure $ String p $ BS.init $ BS.tail $ BS.take len s }

{
tok :: (AlexPosn -> token) -> AlexInput -> Int64 -> Alex token
tok ctor (p, _, _, _) _ = pure $ ctor p

data LexError = LexError
  { leLine :: Int
  , leColumn :: Int
  , leFile :: Maybe FilePath
  } deriving (Show)

instance Exception LexError where
  displayException (LexError l c f) = concat
    [ "lexical error at line "
    , show l
    , ", column "
    , show c
    , maybe "" (", at " <>) f
    ]

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
  (pos, _, _, _) <- alexGetInput
  pure $ EOF pos
}
