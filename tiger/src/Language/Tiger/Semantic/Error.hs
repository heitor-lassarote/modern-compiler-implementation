module Language.Tiger.Semantic.Error
  ( ArityMismatch (..)
  , AssignedToForVar (..)
  , FieldMismatch (..)
  , IllegalBreak (..)
  , IllegalNil (..)
  , InequalityUnsupportedTypes (..)
  , NotAFunction (..)
  , TypeMismatch (..)
  , UndefinedSymbol (..)
  , UnknownField (..)
  , UnsupportedFirstClassFunction (..)
  , arityMismatch
  , assignedToForVar
  , fieldMismatch
  , illegalBreak
  , illegalNil
  , inequalityUnsupportedTypes
  , notAFunction
  , typeMismatch
  , undefinedSymbol
  , unknownField
  , unsupportedFirstClassFunction

  , panic
  ) where

import Control.Exception (Exception (..), throw)
import GHC.Stack (HasCallStack)

import Language.Tiger.AST (BinOp, Name)
import Language.Tiger.Position (Range)
import Language.Tiger.Semantic.Types (Symbol, Type (..))

displayError :: Range -> String -> String
displayError range name = "Error at " <> show range <> ":\n" <> name <> ": "

panic :: HasCallStack => String -> a
panic msg = error $ "impossible! " <> msg

data ArityMismatch = ArityMismatch Name Int Int Range
  deriving stock (Eq, Show)

instance Exception ArityMismatch where
  displayException (ArityMismatch name expected actual range) =
    displayError range "Arity mismatch"
    <> "The function " <> show name <> " expects " <> show expected <> " arguments, but " <> show actual <> " were given."

data AssignedToForVar = AssignedToForVar Name Range
  deriving stock (Eq, Show)

instance Exception AssignedToForVar where
  displayException (AssignedToForVar name range) =
    displayError range "Attempt to assign to non-assignable variable bound in for"
    <> show name <> " can't be assigned to, it was bound by a for loop."

data FieldMismatch = FieldMismatch (Symbol, Type) (Symbol, Type) Range
  deriving stock (Eq, Show)

instance Exception FieldMismatch where
  displayException (FieldMismatch (expectedName, expectedType) (actualName, actualType) range) =
    displayError range "Field mismatch"
    <> "Expected a field with name" <> show expectedName <> " of type " <> show expectedType <> ", but got a field with name " <> show actualName <> " of type " <> show actualType <> "."

newtype IllegalBreak = IllegalBreak Range
  deriving stock (Eq, Show)

instance Exception IllegalBreak where
  displayException (IllegalBreak range) =
    displayError range "Illegal usage of Break"
    <> "`Break` is not allowed to appear outside a `for` or `while` loop."

newtype IllegalNil = IllegalNil Range
  deriving stock (Eq, Show)

instance Exception IllegalNil where
  displayException (IllegalNil range) =
    displayError range "Illegal usage of Nil"
    <> "`Nil` is not allowed to appear in this expression."

data InequalityUnsupportedTypes = InequalityUnsupportedTypes (BinOp Range) Type Type Range
  deriving stock (Eq, Show)

instance Exception InequalityUnsupportedTypes where
  displayException (InequalityUnsupportedTypes op left right range) =
    displayError range "Unsupported types on inequality"
    <> "The operator " <> show op <> " only supports the types `int` and `string`, but the LHS has type " <> show left <> ". The RHS has type " <> show right <> "."

data TypeMismatch = TypeMismatch Type Type Range
  deriving stock (Eq, Show)

instance Exception TypeMismatch where
  displayException (TypeMismatch expected actual range) =
    displayError range "Type mismatch"
    <> "Expected " <> show expected <> ", but got " <> show actual <> "."

data NotAFunction = NotAFunction Name Type Range
  deriving stock (Eq, Show)

instance Exception NotAFunction where
  displayException (NotAFunction name typ range) =
    displayError range "Not a function"
    <> show name <> " of type " <> show typ <> " is not a function."

data UndefinedSymbol = UndefinedSymbol Name Range
  deriving stock (Eq, Show)

instance Exception UndefinedSymbol where
  displayException (UndefinedSymbol name range) =
    displayError range "Undefined symbol"
    <> show name <> " is not in scope."

data UnknownField = UnknownField Name Type Range
  deriving stock (Eq, Show)

instance Exception UnknownField where
  displayException (UnknownField name typ range) =
    displayError range "Unknown field"
    <> "No field called " <> show name <> " of type " <> show typ <> " could be found."

data UnsupportedFirstClassFunction = UnsupportedFirstClassFunction Name Range
  deriving stock (Eq, Show)

instance Exception UnsupportedFirstClassFunction where
  displayException (UnsupportedFirstClassFunction name range) =
    displayError range "Unsupported first class function"
    <> "Referencing " <> show name <> " is not supported in this context, functions are not first-class."

arityMismatch :: Name -> Int -> Int -> Range -> a
arityMismatch name expected actual range = throw $ ArityMismatch name expected actual range

assignedToForVar :: Name -> Range -> a
assignedToForVar name range = throw $ AssignedToForVar name range

fieldMismatch :: (Symbol, Type) -> (Symbol, Type) -> Range -> a
fieldMismatch expected actual range = throw $ FieldMismatch expected actual range

illegalBreak :: Range -> a
illegalBreak range = throw $ IllegalBreak range

illegalNil :: Range -> a
illegalNil range = throw $ IllegalNil range

inequalityUnsupportedTypes :: BinOp Range -> Type -> Type -> Range -> a
inequalityUnsupportedTypes op left right range = throw $ InequalityUnsupportedTypes op left right range

notAFunction :: Name -> Type -> Range -> a
notAFunction name typ range = throw $ NotAFunction name typ range

typeMismatch :: Type -> Type -> Range -> a
typeMismatch expected actual range = throw $ TypeMismatch expected actual range

undefinedSymbol :: Name -> Range -> a
undefinedSymbol name range = throw $ UndefinedSymbol name range

unknownField :: Name -> Type -> Range -> a
unknownField name typ range = throw $ UnknownField name typ range

unsupportedFirstClassFunction :: Name -> Range -> a
unsupportedFirstClassFunction name range = throw $ UnsupportedFirstClassFunction name range
