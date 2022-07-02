{
module Language.Tiger.Parser
  ( parseTiger, runAlex
  ) where

import Data.Bool (bool)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V

import Language.Tiger.AST
  ( Accessor (..), BinOp (..), Dec (..), Exp (..), Id (..), LValue (..), Lit (..)
  , Name (..), RecField (..), Seq (..), Ty (..), TypeField (..), TypeId (..)
  , getMeta
  )
import Language.Tiger.Lexer (Token (..), runAlex)
import Language.Tiger.Lexer qualified as L
import Language.Tiger.Position (Pos (..), Range (..), (<->))
}

%expect 0

{- Note: [%shift: Exp -> if Exp then Exp]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
  Exp -> if Exp then Exp . else Exp
  Exp -> if Exp then Exp .

Example:
  if b1 then if b2 then a else b

Ambiguity:
  If we reduced, we would get:  if b1 then (if b2 then a) else b
  We shift to get this instead: if b1 then (if b2 then a else b)
-}

%name parseTiger Exp
%tokentype { L.Token }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { Token _ L.EOF }

%token
  array     { Token _ L.Array }
  break     { Token _ L.Break }
  do        { Token _ L.Do }
  else      { Token _ L.Else }
  end       { Token _ L.End }
  for       { Token _ L.For }
  function  { Token _ L.Function }
  id        { Token _ (L.Id _) }
  if        { Token _ L.If }
  in        { Token _ L.In }
  int       { Token _ (L.Int _) }
  let       { Token _ L.Let }
  of        { Token _ L.Of }
  nil       { Token _ L.Nil }
  string    { Token _ (L.String _) }
  then      { Token _ L.Then }
  to        { Token _ L.To }
  type      { Token _ L.Type }
  var       { Token _ L.Var }
  while     { Token _ L.While }
  ':='      { Token _ L.Assign }
  '='       { Token _ L.EQ }
  '<>'      { Token _ L.NEQ }
  '>'       { Token _ L.GT }
  '<'       { Token _ L.LT }
  '>='      { Token _ L.GE }
  '<='      { Token _ L.LE }
  '{'       { Token _ L.LBrace }
  '}'       { Token _ L.RBrace }
  ']'       { Token _ L.RBrack }
  '['       { Token _ L.LBrack }
  ')'       { Token _ L.RParen }
  '('       { Token _ L.LParen }
  ';'       { Token _ L.Semicolon }
  ':'       { Token _ L.Colon }
  ','       { Token _ L.Comma }
  '/'       { Token _ L.Divide }
  '*'       { Token _ L.Times }
  '-'       { Token _ L.Minus }
  '+'       { Token _ L.Plus }
  '.'       { Token _ L.Dot }
  '&'       { Token _ L.And }
  '|'       { Token _ L.Or }

%right in of do else ':='
%left '|'
%left '&'
%nonassoc '=' '<>' '>' '<' '>=' '<='
%left '-' '+'
%left '/' '*'
%left NEG

%%

-- Utilities

sepBy(p, sep)
  :                     { [] }
  | p                   { [$1] }
  | p sep sepBy(p, sep) { $1 : $3 }

many(p)
  :           { [] }
  | p many(p) { $1 : $2 }

optional(p)
  :   { Nothing }
  | p { Just $1 }

-- Declarations

Decs :: { [Dec Range] }
  : many(Dec) { $1 }

Dec :: { Dec Range }
  : TyDec  { $1 }
  | VarDec { $1 }
  | FunDec { $1 }

-- Types

TypeAnnotation :: { TypeId Range }
  : ':' TypeId { $2 }

TypeId :: { TypeId Range }
  : id { TypeId (range $1) (getId $1) }

TyDec :: { Dec Range }
  : type TypeId '=' Ty { TyDec (range $1 <-> getMeta $4) $2 $4 }

Ty :: { Ty Range }
  : TypeId             { TyId (getMeta $1) $1 }
  | '{' TypeFields '}' { TyFields (range $1 <-> range $3) (V.fromList $2) }
  | array of TypeId    { TyArray (range $1 <-> getMeta $3) $3 }

TypeFields :: { [TypeField Range] }
  : sepBy(TypeField, ',') { $1 }

TypeField :: { TypeField Range }
  : Id TypeAnnotation { TypeField (getMeta $1 <-> getMeta $2) $1 $2 }

-- Variables

Id :: { Id Range }
  : id { Id (range $1) (getId $1) }

VarDec :: { Dec Range }
  : var Id optional(TypeAnnotation) ':=' Exp { VarDec (range $1 <-> getMeta $5) $2 $3 $5 }

-- Functions

FunDec :: { Dec Range }
  : function Id '(' TypeFields ')' optional(TypeAnnotation) '=' Exp
    { FunDec (range $1 <-> getMeta $8) $2 (V.fromList $4) $6 $8 }

-- Expressions

Exp :: { Exp Range }
  : Exp '/'  Exp { EBinOp (getMeta $1 <-> getMeta $3) (Divide (range $2)) $1 $3 }
  | Exp '*'  Exp { EBinOp (getMeta $1 <-> getMeta $3) (Times  (range $2)) $1 $3 }
  | Exp '-'  Exp { EBinOp (getMeta $1 <-> getMeta $3) (Minus  (range $2)) $1 $3 }
  | Exp '+'  Exp { EBinOp (getMeta $1 <-> getMeta $3) (Plus   (range $2)) $1 $3 }
  | Exp '='  Exp { EBinOp (getMeta $1 <-> getMeta $3) (Eq     (range $2)) $1 $3 }
  | Exp '<>' Exp { EBinOp (getMeta $1 <-> getMeta $3) (Neq    (range $2)) $1 $3 }
  | Exp '>'  Exp { EBinOp (getMeta $1 <-> getMeta $3) (Gt     (range $2)) $1 $3 }
  | Exp '<'  Exp { EBinOp (getMeta $1 <-> getMeta $3) (Lt     (range $2)) $1 $3 }
  | Exp '>=' Exp { EBinOp (getMeta $1 <-> getMeta $3) (Ge     (range $2)) $1 $3 }
  | Exp '<=' Exp { EBinOp (getMeta $1 <-> getMeta $3) (Le     (range $2)) $1 $3 }
  | Exp '&'  Exp { EBinOp (getMeta $1 <-> getMeta $3) (Conj   (range $2)) $1 $3 }
  | Exp '|'  Exp { EBinOp (getMeta $1 <-> getMeta $3) (Disj   (range $2)) $1 $3 }
  | ExpNeg       { $1 }

ExpNeg :: { Exp Range }
  : '-' Exp0 %prec NEG { ENeg (range $1 <-> getMeta $2) $2 }
  | Exp0               { $1 }

Exp0 :: { Exp Range }
  -- FIXME: We use `id` here and in LValue to avoid a reduce/reduce conflict.
  : id '[' Exp ']' of Exp         { EArray (range $1 <-> getMeta $6) (TypeId (range $1) (getId $1)) $3 $6 }
  | LValue ':=' Exp               { EAssign (getMeta $1 <-> getMeta $3) $1 $3 }
  | if Exp then Exp else Exp      { EIfThenElse (range $1 <-> getMeta $6) $2 $4 $6 }
  | if Exp then Exp %shift        { EIfThen (range $1 <-> getMeta $4) $2 $4 }
  | while Exp do Exp              { EWhile (range $1 <-> getMeta $4) $2 $4 }
  | for Id ':=' Exp to Exp do Exp { EFor (range $1 <-> getMeta $8) $2 $4 $6 $8 }
  | Atom                          { $1 }

Atom :: { Exp Range }
  : LValue                   { ELValue (getMeta $1) $1 }
  | nil                      { ENil (range $1) }
  | Id '(' Args ')'          { ECall (getMeta $1 <-> range $4) $1 (V.fromList $3) }
  | TypeId '{' RecFields '}' { ERecord (getMeta $1 <-> range $4) $1 (V.fromList $3) }
  | '(' Seq ')'              { ESeq (range $1 <-> range $3) $2 }
  | Lit                      { ELit (getMeta $1) $1 }
  | break                    { EBreak (range $1) }
  | let Decs in ExpSeq end   { ELet (range $1 <-> range $5) (V.fromList $2) (V.fromList $4) }
  | '(' ')'                  { EUnit (range $1 <-> range $2) }
  | '(' Exp ')'              { EPar (range $1 <-> range $3) $2 }

LValue :: { LValue Range }
  : id AccessorChain
    {
      let chain = V.fromList $2 in
      LValue (range $1 <-> getVecMeta (range $1) chain) (Id (range $1) (getId $1)) chain
    }

AccessorChain :: { [Accessor Range] }
  : many(Accessor) { $1 }

Accessor :: { Accessor Range }
  : '.' Id      { AccessorRecField (range $1 <-> getMeta $2) $2 }
  | '[' Exp ']' { AccessorArraySub (range $1 <-> range $3) $2 }

Args :: { [Exp Range] }
  : sepBy(Exp, ',') { $1 }

RecFields :: { [RecField Range] }
  : sepBy(RecField, ',') { $1 }

RecField :: { RecField Range }
  : Id '=' Exp { RecField (getMeta $1 <-> getMeta $3) $1 $3 }

Seq :: { Seq Range }
  : Exp ';' Seq { SeqSpine (getMeta $1 <-> getMeta $3) $1 $3 }
  | Exp ';' Exp { SeqNil (getMeta $1 <-> getMeta $3) $1 $3 }

Lit :: { Lit Range }
  : int    { LInt (range $1) (getInt $1) }
  | string { LString (range $1) (getString $1) }

ExpSeq :: { [Exp Range] }
  : sepBy(Exp, ';') { $1 }

{
parseError :: L.Token -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.Token -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

getId :: Token -> Name
getId (Token _ (L.Id i)) = Name $ T.decodeUtf8 $ BS.toStrict i

getInt :: Token -> Integer
getInt (Token _ (L.Int i)) = i

getString :: Token -> Text
getString (Token _ (L.String s)) = T.decodeUtf8 $ BS.toStrict s

getVecMeta :: Foldable f => a -> (Vector (f a)) -> a
getVecMeta fallback vec = bool (getMeta $ V.last vec) fallback $ null vec
}
