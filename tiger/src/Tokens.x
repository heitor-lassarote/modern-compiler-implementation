{
module Tokens where

import Prelude hiding (Ordering (..))

import Control.Exception (Exception (..))
import Data.List.NonEmpty (NonEmpty (..), (<|))
}

%wrapper "posn"

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

type { \p s -> Type p }
var { \p s -> Var p }
function { \p s -> Function p }
break { \p s -> Break p }
of { \p s -> Of p }
end { \p s -> End p }
in { \p s -> In p }
nil { \p s -> Nil p }
let { \p s -> Let p }
do { \p s -> Do p }
to { \p s -> To p }
for { \p s -> For p }
while { \p s -> While p }
else { \p s -> Else p }
then { \p s -> Then p }
if { \p s -> If p }
array { \p s -> Array p }
":=" { \p s -> Assign p }
"|" { \p s -> Or p }
"&" { \p s -> And p }
">=" { \p s -> GE p }
">" { \p s -> GT p }
"<=" { \p s -> LE p }
"<" { \p s -> LT p }
"<>" { \p s -> NEQ p }
"=" { \p s -> EQ p }
"/" { \p s -> Divide p }
"*" { \p s -> Times p }
"-" { \p s -> Minus p }
"+" { \p s -> Plus p }
"." { \p s -> Dot p }
"}" { \p s -> RBrace p }
"{" { \p s -> LBrace p }
"]" { \p s -> RBrack p }
"[" { \p s -> LBrack p }
")" { \p s -> RParen p }
"(" { \p s -> LParen p }
";" { \p s -> Semicolon p }
":" { \p s -> Colon p }
"," { \p s -> Comma p }
$digit+ { \p s -> Int p (read s) }
@id { \p s -> Id p s }

@string { \p s -> String p (init $ tail s) }

{
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

-- | Runs the lexer, returning a list of tokens. Appends EOF at the end.
--
-- Taken from the generated output, see alexScanTokens
runLexer :: String -> Either LexError (NonEmpty Token)
-- TODO: Consider using transformers instead
runLexer str0 = sequenceA $ go (alexStartPos, '\n', [], str0)
  where
    go inp@(pos, _, _, str) = case alexScan inp 0 of
      AlexEOF -> Right (EOF pos) :| []
      AlexError ((AlexPn _ line column), _, _, _) -> Left (LexError line column Nothing) :| []
      AlexSkip  inp' _ln -> go inp'
      AlexToken inp' len act -> Right (act pos (take len str)) <| go inp'

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
  | String AlexPosn String
  | Int AlexPosn Integer
  | Id AlexPosn String
  | EOF AlexPosn
  deriving (Eq, Show)
}
