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
  , BinOp (..), Exp (..), LValue (..), Seq (..), Lit (..), RecField (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import Language.Tiger.Lexer (Token (..))
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
  array     { $$@(Token _ L.Array) }
  break     { $$@(Token _ L.Break) }
  do        { $$@(Token _ L.Do) }
  else      { $$@(Token _ L.Else) }
  end       { $$@(Token _ L.End) }
  for       { $$@(Token _ L.For) }
  function  { $$@(Token _ L.Function) }
  id        { $$@(Token _ (L.Id _)) }
  if        { $$@(Token _ L.If) }
  in        { $$@(Token _ L.In) }
  int       { $$@(Token _ (L.Int _)) }
  let       { $$@(Token _ L.Let) }
  of        { $$@(Token _ L.Of) }
  nil       { $$@(Token _ L.Nil) }
  string    { $$@(Token _ (L.String _)) }
  then      { $$@(Token _ L.Then) }
  to        { $$@(Token _ L.To) }
  type      { $$@(Token _ L.Type) }
  var       { $$@(Token _ L.Var) }
  while     { $$@(Token _ L.While) }
  ':='      { $$@(Token _ L.Assign) }
  '='       { $$@(Token _ L.EQ) }
  '<>'      { $$@(Token _ L.NEQ) }
  '>'       { $$@(Token _ L.GT) }
  '<'       { $$@(Token _ L.LT) }
  '>='      { $$@(Token _ L.GE) }
  '<='      { $$@(Token _ L.LE) }
  '{'       { $$@(Token _ L.LBrace) }
  '}'       { $$@(Token _ L.RBrace) }
  ']'       { $$@(Token _ L.RBrack) }
  '['       { $$@(Token _ L.LBrack) }
  ')'       { $$@(Token _ L.RParen) }
  '('       { $$@(Token _ L.LParen) }
  ';'       { $$@(Token _ L.Semicolon) }
  ':'       { $$@(Token _ L.Colon) }
  ','       { $$@(Token _ L.Comma) }
  '/'       { $$@(Token _ L.Divide) }
  '*'       { $$@(Token _ L.Times) }
  '-'       { $$@(Token _ L.Minus) }
  '+'       { $$@(Token _ L.Plus) }
  '.'       { $$@(Token _ L.Dot) }
  '&'       { $$@(Token _ L.And) }
  '|'       { $$@(Token _ L.Or) }

%right in
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
  | '{' TypeFields '}' { TyFields (range $1 <-> range $3) $2 }
  | array of TypeId    { TyArray (range $1 <-> getMeta $3) $3 }

TypeFields :: { [TypeField Range] }
  : sepBy(TypeField, ',') { $1 }

TypeField :: { TypeField Range }
  : Id TypeAnnotation { TypeField (getMeta $1 <-> getMeta $2) $1 $2 }

-- Variables

Id :: { Id Range }
  : id { Id (range $1) (getId $1) }

VarDec :: { Dec Range }
  : var Id ':=' Exp { VarDec (range $1 <-> getMeta $4) $2 $4 }

-- Functions

FunDec :: { Dec Range }
  : function Id '(' TypeFields ')' optional(TypeAnnotation) '=' Exp { FunDec (range $1 <-> getMeta $8) $2 $4 $6 $8 }

-- Expressions

BinOp :: { BinOp Range }
  : '/'  { Divide (range $1) }
  | '*'  { Times  (range $1) }
  | '-'  { Minus  (range $1) }
  | '+'  { Plus   (range $1) }
  | '='  { Eq     (range $1) }
  | '<>' { Neq    (range $1) }
  | '>'  { Gt     (range $1) }
  | '<'  { Lt     (range $1) }
  | '>=' { Ge     (range $1) }
  | '<=' { Le     (range $1) }
  | '&'  { Conj   (range $1) }
  | '|'  { Disj   (range $1) }

Exp :: { Exp Range }
  : Atom BinOp Exp { EBinOp (getMeta $1 <-> getMeta $3) $2 $1 $3 }
  | ExpNeg         { $1 }

ExpNeg :: { Exp Range }
ExpNeg
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
  | Id '(' Args ')'          { ECall (getMeta $1 <-> range $4) $1 $3 }
  | TypeId '{' RecFields '}' { ERecord (getMeta $1 <-> range $4) $1 $3 }
  | '(' Seq ')'              { ESeq (range $1 <-> range $3) $2 }
  | Lit                      { ELit (getMeta $1) $1 }
  | break                    { EBreak (range $1) }
  | let Decs in ExpSeq end   { ELet (range $1 <-> range $5) $2 $4 }
  | '(' ')'                  { EUnit (range $1 <-> range $2) }
  | '(' Exp ')'              { EPar (range $1 <-> range $3) $2 }

LValue :: { LValue Range }
  : id AccessorChain { LValue (range $1 <-> getListMeta (range $1) $2) (Id (range $1) (getId $1)) $2 }

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

getId :: Token -> ByteString
getId (Token _ (L.Id i)) = i

getInt :: Token -> Integer
getInt (Token _ (L.Int i)) = i

getString :: Token -> ByteString
getString (Token _ (L.String s)) = s

getMeta :: Foldable f => f a -> a
getMeta = fromJust . getFirst . foldMap pure

getListMeta :: Foldable f => a -> [f a] -> a
getListMeta fallback = maybe fallback (getMeta . NE.last) . NE.nonEmpty

data Id a
  = Id a ByteString
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data TypeId a
  = TypeId a ByteString
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data Dec a
  = TyDec a (TypeId a) (Ty a)
  | VarDec a (Id a) (Exp a)
  | FunDec a (Id a) [TypeField a] (Maybe (TypeId a)) (Exp a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data Ty a
  = TyId a (TypeId a)
  | TyFields a [TypeField a]
  | TyArray a (TypeId a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data TypeField a
  = TypeField a (Id a) (TypeId a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data BinOp a
  = Divide a
  | Times a
  | Minus a
  | Plus a
  | Eq a
  | Neq a
  | Lt a
  | Gt a
  | Ge a
  | Le a
  | Conj a
  | Disj a
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data Exp a
  = ELValue a (LValue a)
  | ENil a
  | ESeq a (Seq a)
  | EUnit a
  | ELit a (Lit a)
  | ENeg a (Exp a)
  | ECall a (Id a) [Exp a]
  | EBinOp a (BinOp a) (Exp a) (Exp a)
  | ERecord a (TypeId a) [RecField a]
  | EArray a (TypeId a) (Exp a) (Exp a)
  | EAssign a (LValue a) (Exp a)
  | EIfThenElse a (Exp a) (Exp a) (Exp a)
  | EIfThen a (Exp a) (Exp a)
  | EWhile a (Exp a) (Exp a)
  | EFor a (Id a) (Exp a) (Exp a) (Exp a)
  | EBreak a
  | ELet a [Dec a] [Exp a]
  | EPar a (Exp a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data LValue a
  = LValue a (Id a) [Accessor a]
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data Accessor a
  = AccessorRecField a (Id a)
  | AccessorArraySub a (Exp a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data Seq a
  = SeqSpine a (Exp a) (Seq a)
  | SeqNil a (Exp a) (Exp a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data Lit a
  = LInt a Integer
  | LString a ByteString
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data RecField a
  = RecField a (Id a) (Exp a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)
}
