{
module Language.Tiger.Parser
  ( parseTiger

  -- * Variable and type IDs
  , Id (..), TypeId (..)

  -- * Declarations
  , Dec (..)

  -- * Types
  , Ty (..), TypeField (..)

  -- * Expresions
  , Exp (..), LValue (..), Seq (..), Lit (..), RecField (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

import Language.Tiger.Lexer qualified as L
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
%lexer { lexer } { L.EOF _ }

%token
  array     { L.Array _ }
  break     { L.Break _ }
  do        { L.Do _ }
  else      { L.Else _ }
  end       { L.End _ }
  for       { L.For _ }
  function  { L.Function _ }
  id        { L.Id _ $$ }
  if        { L.If _ }
  in        { L.In _ }
  int       { L.Int _ $$ }
  let       { L.Let _ }
  of        { L.Of _ }
  nil       { L.Nil _ }
  string    { L.String _ $$ }
  then      { L.Then _ }
  to        { L.To _ }
  type      { L.Type _ }
  var       { L.Var _ }
  while     { L.While _ }
  ':='      { L.Assign _ }
  '='       { L.EQ _ }
  '<>'      { L.NEQ _ }
  '>'       { L.GT _ }
  '<'       { L.LT _ }
  '>='      { L.GE _ }
  '<='      { L.LE _ }
  '{'       { L.LBrace _ }
  '}'       { L.RBrace _ }
  ']'       { L.RBrack _ }
  '['       { L.LBrack _ }
  ')'       { L.RParen _ }
  '('       { L.LParen _ }
  ';'       { L.Semicolon _ }
  ':'       { L.Colon _ }
  ','       { L.Comma _ }
  '/'       { L.Divide _ }
  '*'       { L.Times _ }
  '-'       { L.Minus _ }
  '+'       { L.Plus _ }
  '.'       { L.Dot _ }
  '&'       { L.And _ }
  '|'       { L.Or _ }

%right in
%left '|'
%left '&'
%nonassoc '=' '<>' '>' '<' '>=' '<='
%left '-' '+'
%left '/' '*'
%left NEG

%%

-- Declarations

Decs :: { [Dec] }
  :          { [] }
  | Dec Decs { $1 : $2 }

Dec :: { Dec }
  : TyDec  { $1 }
  | VarDec { $1 }
  | FunDec { $1 }

-- Types

TypeId :: { TypeId }
  : id { TypeId $1 }

TyDec :: { Dec }
  : type TypeId '=' Ty { TyDec $2 $4 }

Ty :: { Ty }
  : TypeId             { TyId $1 }
  | '{' TypeFields '}' { TyFields $2 }
  | array of TypeId    { TyArray $3 }

TypeFields :: { [TypeField] }
  :                          { [] }
  | TypeField                { [$1] }
  | TypeField ',' TypeFields { $1 : $3 }

TypeField :: { TypeField }
  : Id ':' TypeId { TypeField $1 $3 }

-- Variables

Id :: { Id }
  : id { Id $1 }

VarDec :: { Dec }
  : var Id ':=' Exp { VarDec $2 $4 }

-- Functions

FunDec :: { Dec }
  : function Id '(' TypeFields ')'            '=' Exp { FunDec $2 $4 Nothing   $7 }
  | function Id '(' TypeFields ')' ':' TypeId '=' Exp { FunDec $2 $4 (Just $7) $9 }

-- Expressions

Exp :: { Exp }
  : Atom '/'  Exp { EDivide $1 $3 }
  | Atom '*'  Exp { ETimes $1 $3 }
  | Atom '-'  Exp { EMinus $1 $3 }
  | Atom '+'  Exp { EPlus $1 $3 }
  | Atom '='  Exp { EEq $1 $3 }
  | Atom '<>' Exp { ENeq $1 $3 }
  | Atom '>'  Exp { EGt $1 $3 }
  | Atom '<'  Exp { ELt $1 $3 }
  | Atom '>=' Exp { EGe $1 $3 }
  | Atom '<=' Exp { ELe $1 $3 }
  | Atom '&'  Exp { EConj $1 $3 }
  | Atom '|'  Exp { EDisj $1 $3 }
  | ExpNeg        { $1 }

ExpNeg :: { Exp }
ExpNeg
  : '-' Exp0 %prec NEG { ENeg $2 }
  | Exp0               { $1 }

Exp0 :: { Exp }
  -- FIXME: We use `id` here and in LValue to avoid a reduce/reduce conflict.
  : id '[' Exp ']' of Exp         { EArray (TypeId $1) $3 $6 }
  | LValue ':=' Exp               { EAssign $1 $3 }
  | if Exp then Exp else Exp      { EIfThenElse $2 $4 $6 }
  | if Exp then Exp %shift        { EIfThen $2 $4 }
  | while Exp do Exp              { EWhile $2 $4 }
  | for Id ':=' Exp to Exp do Exp { EFor $2 $4 $6 $8 }
  | Atom                          { $1 }

Atom :: { Exp }
  : LValue                   { ELValue $1 }
  | nil                      { ENil }
  | Id '(' Args ')'          { ECall $1 $3 }
  | TypeId '{' RecFields '}' { ERecord $1 $3 }
  | '(' Seq ')'              { ESeq $2 }
  | Lit                      { ELit $1 }
  | break                    { EBreak }
  | let Decs in ExpSeq end   { ELet $2 $4 }
  | '(' ')'                  { EUnit }
  | '(' Exp ')'              { EPar $2 }

LValue :: { LValue }
  : id AccessorChain { LValue (Id $1) $2 }

AccessorChain :: { [Accessor] }
  :                        { [] }
  | Accessor AccessorChain { $1 : $2 }

Accessor :: { Accessor }
  : '.' Id      { AccessorRecField $2 }
  | '[' Exp ']' { AccessorArraySub $2 }

Args :: { [Exp] }
  :              { [] }
  | Exp          { [$1] }
  | Exp ',' Args { $1 : $3 }

RecFields :: { [RecField] }
  :                        { [] }
  | RecField               { [$1] }
  | RecField ',' RecFields { $1 : $3 }

RecField :: { RecField }
  : Id '=' Exp { RecField $1 $3 }

Seq :: { Seq }
  : Exp ';' Seq { SeqSpine $1 $3 }
  | Exp ';' Exp { SeqNil $1 $3 }

Lit :: { Lit }
  : int    { LInt $1 }
  | string { LString $1 }

ExpSeq :: { [Exp] }
  :                { [] }
  | Exp            { [$1] }
  | Exp ';' ExpSeq { $1 : $3 }

{
parseError :: L.Token -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.Token -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

newtype Id
  = Id ByteString
  deriving stock (Eq, Show)

newtype TypeId
  = TypeId ByteString
  deriving stock (Eq, Show)

data Dec
  = TyDec  TypeId Ty
  | VarDec Id Exp
  | FunDec Id [TypeField] (Maybe TypeId) Exp
  deriving stock (Eq, Show)

data Ty
  = TyId TypeId
  | TyFields [TypeField]
  | TyArray TypeId
  deriving stock (Eq, Show)

data TypeField
  = TypeField Id TypeId
  deriving stock (Eq, Show)

data Exp
  = ELValue LValue
  | ENil
  | ESeq Seq
  | EUnit
  | ELit Lit
  | ENeg Exp
  | ECall Id [Exp]
  | EDivide Exp Exp
  | ETimes Exp Exp
  | EMinus Exp Exp
  | EPlus Exp Exp
  | EEq Exp Exp
  | ENeq Exp Exp
  | ELt Exp Exp
  | EGt Exp Exp
  | EGe Exp Exp
  | ELe Exp Exp
  | EConj Exp Exp
  | EDisj Exp Exp
  | ERecord TypeId [RecField]
  | EArray TypeId Exp Exp
  | EAssign LValue Exp
  | EIfThenElse Exp Exp Exp
  | EIfThen Exp Exp
  | EWhile Exp Exp
  | EFor Id Exp Exp Exp
  | EBreak
  | ELet [Dec] [Exp]
  | EPar Exp
  deriving stock (Eq, Show)

data LValue
  = LValue Id [Accessor]
  deriving stock (Eq, Show)

data Accessor
  = AccessorRecField Id
  | AccessorArraySub Exp
  deriving stock (Eq, Show)

data Seq
  = SeqSpine Exp Seq
  | SeqNil Exp Exp
  deriving stock (Eq, Show)

data Lit
  = LInt Integer
  | LString ByteString
  deriving stock (Eq, Show)

data RecField
  = RecField Id Exp
  deriving stock (Eq, Show)
}
