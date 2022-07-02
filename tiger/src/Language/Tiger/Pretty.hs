{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Tiger.Pretty
  (
  ) where

import Data.Char (chr, ord)
import Data.Ix (inRange)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Prettyprinter (Doc, Pretty (..), (<+>))
import Prettyprinter qualified as PP

import Language.Tiger.AST
  ( Accessor (..), BinOp (..), Dec (..), Exp (..), Id (..), LValue (..), Lit (..)
  , Name (..), RecField (..), Seq (..), Ty (..), TypeField (..), TypeId (..)
  )

indent :: Doc ann -> Doc ann
indent = PP.indent 2

instance Pretty Name where
  pretty (Name name) = pretty name

instance Pretty (Id a) where
  pretty (Id _ name) = pretty name

instance Pretty (TypeId a) where
  pretty (TypeId _ name) = pretty name

prettyTypeAnnotation :: Maybe (TypeId a) -> Doc ann
prettyTypeAnnotation = maybe "" ((PP.colon <+>) . pretty)

instance Pretty (Dec a) where
  pretty = \case
    TyDec _ tyName ty ->
      "type" <+> pretty tyName <+> PP.equals <> PP.softline <> pretty ty
    VarDec _ id' tyM expr ->
      "var" <+> pretty id' <+> prettyTypeAnnotation tyM <+> ":=" <> PP.softline <> pretty expr
    FunDec _ id' fields retM expr -> PP.vsep
      [ "function" <+> pretty id' <> PP.tupled (pretty <$> V.toList fields)
        <> prettyTypeAnnotation retM <+> PP.equals
      , indent (pretty expr)
      ]

instance Pretty (Ty a) where
  pretty = \case
    TyId _ ty -> pretty ty
    TyFields _ fields -> PP.group $ PP.encloseSep lbrace rbrace sep $ pretty <$> V.toList fields
      where
        lbrace, rbrace, sep :: Doc ann
        lbrace = PP.flatAlt "{ " "{"
        rbrace = PP.flatAlt " }" "}"
        sep    = ", "
    TyArray _ ty -> "array" <+> "of" <+> pretty ty

instance Pretty (TypeField a) where
  pretty (TypeField _ id' tyId) = pretty id' <> PP.colon <+> pretty tyId

instance Pretty (BinOp a) where
  pretty = \case
    Divide _ -> "/"
    Times  _ -> "*"
    Minus  _ -> "-"
    Plus   _ -> "+"
    Eq     _ -> "="
    Neq    _ -> "<>"
    Lt     _ -> "<"
    Gt     _ -> ">"
    Le     _ -> "<="
    Ge     _ -> ">="
    Conj   _ -> "&"
    Disj   _ -> "|"

instance Pretty (Exp a) where
  pretty = \case
    ELValue _ lValue -> pretty lValue
    ENil _ -> "nil"
    ESeq _ exprs -> PP.lparen <> pretty exprs <> PP.rparen
    EUnit _ -> PP.lparen <> PP.rparen
    ELit _ lit -> pretty lit
    ENeg _ expr -> "-" <> pretty expr
    ECall _ id' args -> pretty id' <> PP.parens (PP.concatWith (PP.surround ", ") (pretty <$> args))
    EBinOp _ op left right -> pretty left <+> pretty op <+> pretty right
    ERecord _ idTy fields -> pretty idTy <> PP.braces (prettyRecFields fields)
    EArray _ idTy count value -> pretty idTy <> PP.brackets (pretty count) <+> "of" <+> pretty value
    EAssign _ lvalue expr -> pretty lvalue <+> ":=" <+> pretty expr
    EIfThenElse _ if' then' else' -> PP.vsep
      [ "if" <+> pretty if'
      , "then"
      , indent (pretty then')
      , "else"
      , indent (pretty else')
      ]
    EIfThen _ if' then' -> PP.vsep
      [ "if" <+> pretty if'
      , "then"
      , indent (pretty then')
      ]
    EWhile _ while' do' -> PP.vsep
      [ "while" <+> pretty while' <+> "do"
      , indent (pretty do')
      ]
    EFor _ id' from to do' -> PP.vsep
      [ "for" <+> pretty id' <+> ":=" <+> pretty from <+> "to" <+> pretty to <+> "do"
      , indent (pretty do')
      ]
    EBreak _ -> "break"
    ELet _ decs exprs -> PP.vsep
      [ "let"
      , indent (PP.vsep (pretty <$> V.toList decs))
      , "in"
      , indent (PP.vsep $ PP.punctuate PP.semi (pretty <$> V.toList exprs))
      , "end"
      ]
    EPar _ expr -> PP.parens (pretty expr)

instance Pretty (LValue a) where
  pretty (LValue _ id' accessorChain) = pretty id' <> foldMap pretty accessorChain

instance Pretty (Accessor a) where
  pretty = \case
    AccessorRecField _ field -> PP.dot <> pretty field
    AccessorArraySub _ index -> PP.brackets (pretty index)

instance Pretty (Seq a) where
  pretty = \case
    SeqSpine _ expr exprs -> pretty expr <> PP.semi <> PP.line <> pretty exprs
    SeqNil _ expr expr' -> PP.vsep
      [ pretty expr <> PP.semi
      , pretty expr'
      ]

instance Pretty (Lit a) where
  pretty = \case
    LInt _ i -> PP.pretty i
    LString _ s ->
      -- Undo all sorts of transformations done by the lexer. Ugly but does the job.
      PP.dquotes $ PP.pretty $ T.concatMap
        (\case
          '\n' -> T.pack "\\n"
          '\t' -> T.pack "\\t"
          '\\' -> T.pack "\\\\"
          '"'  -> T.pack "\\\""
          c | inRange ('\^@', '\^_') c -> T.pack ['\\', '^', chr (ord c + ord '@')]
            | otherwise -> T.singleton c)
        (T.tail $ T.init s)

prettyRecFields :: Vector (RecField a) -> Doc ann
prettyRecFields = PP.concatWith (PP.surround ", ") . fmap pretty

instance Pretty (RecField a) where
  pretty (RecField _ id' expr) = pretty id' <+> PP.equals <+> pretty expr
