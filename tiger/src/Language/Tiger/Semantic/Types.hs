module Language.Tiger.Semantic.Types
  ( Symbol (..)
  , Type (..)
  ) where

import Data.IORef (IORef)
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
  | Name Symbol (IORef (Maybe Type))
  deriving stock (Eq)

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
    Name sym _ -> showParen (d > appPrec)
      $ showString "Name "
      . showsPrec (appPrec + 1) sym
