{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

module StraightLine
  ( Id
  , BinOp (..)
  , Stm (..)
  , Exp (..)
  , prog
  , maxargs
  , interp
  ) where

import Control.Exception (Exception (..), throwIO)
import Data.Functor (void)

type Id = String

data BinOp = Plus | Minus | Times | Div

data Stm
  = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]

data Exp
  = IdExp Id
  | NumExp Int
  | OpExp Exp BinOp Exp
  | ESeqExp Stm Exp

prog :: Stm
prog =
  CompoundStm (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
  $ CompoundStm
    (AssignStm "b" (ESeqExp
      (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
      (OpExp (NumExp 10) Times (IdExp "a"))))
    (PrintStm [IdExp "b"])

-- Program 1:
maxargs :: Stm -> Int
maxargs (CompoundStm a b) = max (maxargs a) (maxargs b)
maxargs (AssignStm _ exp) = maxargsE exp
maxargs (PrintStm exps) = max (length exps) (maximum (maxargsE <$> exps))

maxargsE :: Exp -> Int
maxargsE (IdExp _) = 0
maxargsE (NumExp _) = 0
maxargsE (OpExp a _ b) = max (maxargsE a) (maxargsE b)
maxargsE (ESeqExp s e) = max (maxargs s) (maxargsE e)

-- Program 2:
type Table = [(Id, Int)]

newtype UndefinedIdentifierException = UndefinedIdentifierException Id
  deriving stock (Show)

instance Exception UndefinedIdentifierException where
  displayException (UndefinedIdentifierException i) =
    "Undefined identifier: " <> i

interp :: Stm -> IO ()
interp = void . interpStm []
  where
    interpStm :: Table -> Stm -> IO Table
    interpStm t0 (CompoundStm a b) = do
      t1 <- interpStm t0 a
      interpStm t1 b
    interpStm t0 (AssignStm i e) = do
      (result, t1) <- interpExp t0 e
      pure $ (i, result) : t1
    interpStm t0 (PrintStm exps) = do
      (results, t1) <- accumExps t0 exps
      putStrLn $ unwords $ map show results
      pure t1

    accumExps :: Table -> [Exp] -> IO ([Int], Table)
    accumExps t0 [] = pure ([], t0)
    accumExps t0 (exp : exps) = do
      (result1, t1) <- interpExp t0 exp
      (results, t2) <- accumExps t1 exps
      pure (result1 : results, t2)

    interpExp :: Table -> Exp -> IO (Int, Table)
    interpExp t0 (IdExp i) =
      maybe (throwIO $ UndefinedIdentifierException i) (pure . (, t0)) $ lookup i t0
    interpExp t0 (NumExp n) =
      pure (n, t0)
    interpExp t0 (OpExp l op r) = do
      (resultL, t1) <- interpExp t0 l
      (resultR, t2) <- interpExp t1 r
      let
        (@@) = case op of
          Plus  -> (+)
          Minus -> (-)
          Times -> (*)
          Div   -> div
      pure (resultL @@ resultR, t2)
    interpExp t0 (ESeqExp s e) = do
      t1 <- interpStm t0 s
      interpExp t1 e
