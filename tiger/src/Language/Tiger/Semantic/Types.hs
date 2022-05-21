module Language.Tiger.Semantic.Types
  ( Symbol (..)
  , Type (..)
  ) where

import Data.Unique (Unique)
import Data.Vector (Vector)

import Language.Tiger.AST (Name)

data Symbol = Symbol
  { name :: Name
  , offset :: Int
  } deriving stock (Eq, Show)

data Type
  = Int
  | String
  | Record Unique (Vector (Symbol, Type))
  | Array Unique Type
  | Nil
  | Unit
  | Name Symbol (Maybe Type)

instance Eq Type where
  Int == Int = True
  String == String = True
  Record u1 _ == Record u2 _ = u1 == u2
  Array u1 _ == Array u2 _ = u1 == u2
  Nil == Nil = True
  Unit == Unit = True
  Name s1 t1M == Name s2 t2M = s1 == s2 && case (t1M, t2M) of
    (Just t1, Just t2) -> t1 == t2
    _ -> False
  _ == _ = False

appPrec :: Int
appPrec = 10

instance Show Type where
  showsPrec d = \case
    Int -> showString "Int"
    String -> showString "String"
    Record _ fields -> showParen (d > appPrec)
      $ showString "Record "
      . showsPrec (appPrec + 1) fields
    Array _ ty -> showParen (d > appPrec)
      $ showString "Array "
      . showsPrec (appPrec + 1) ty
    Nil -> showString "Nil"
    Unit -> showString "Unit"
    Name sym tyM -> showParen (d > appPrec)
      $ showString "Name "
      . showsPrec (appPrec + 1) sym
      . showsPrec (appPrec + 1) tyM
