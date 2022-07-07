module Language.Tiger.AST
  ( -- * Variable and type IDs
    Name (..), Id (..), TypeId (..)

    -- * Declarations
  , Dec (..)

    -- * Types
  , Ty (..), TypeField (..)

    -- * Expresions
  , BinOp (..), Exp (..), LValue (..), Accessor (..), Seq (..), Lit (..), RecField (..)

    -- * Utilities
  , getMeta
  ) where

import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector (Vector)

newtype Name
  = Name Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, IsString)

data Id a
  = Id a Name
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data TypeId a
  = TypeId a Name
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data Dec a
  = TyDec a (TypeId a) (Ty a)
  | VarDec a (Id a) (Maybe (TypeId a)) (Exp a)
  | FunDec a (Id a) (Vector (TypeField a)) (Maybe (TypeId a)) (Exp a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data Ty a
  = TyId a (TypeId a)
  | TyFields a (Vector (TypeField a))
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
  | Le a
  | Ge a
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
  | ECall a (Id a) (Vector (Exp a))
  | EBinOp a (BinOp a) (Exp a) (Exp a)
  | ERecord a (TypeId a) (Vector (RecField a))
  | EArray a (TypeId a) (Exp a) (Exp a)
  | EAssign a (LValue a) (Exp a)
  | EIfThenElse a (Exp a) (Exp a) (Exp a)
  | EIfThen a (Exp a) (Exp a)
  | EWhile a (Exp a) (Exp a)
  | EFor a (Id a) (Exp a) (Exp a) (Exp a)
  | EBreak a
  | ELet a (Vector (Dec a)) (Vector (Exp a))
  | EPar a (Exp a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data LValue a
  = LValue a (Id a) (Vector (Accessor a))
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
  | LString a Text
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data RecField a
  = RecField a (Id a) (Exp a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

-- * Utilities

getMeta :: Foldable f => f a -> a
getMeta = fromJust . getFirst . foldMap pure
