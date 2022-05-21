module Language.Tiger.Semantic.TypeChecker
  ( Tc, runTc, TcState, initTcState, TcEnv, initTcEnv
  , transProg
  ) where

import Prelude hiding (exp)

import Control.Monad (void, when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local)
import Control.Monad.State (MonadState, StateT, execStateT, get, put)
import Control.Monad.Trans (lift)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (fold, for_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Traversable (for)
import Data.Unique (newUnique)
import Data.Vector (Vector)
import Data.Vector qualified as V

import Language.Tiger.AST
  ( Accessor (..), BinOp (..), Dec (..), Exp (..), Id (..), Lit (..), LValue (..)
  , Name (..), RecField (..), Seq (..), Ty (..), TypeField (..), TypeId (..)
  , getMeta
  )
import Language.Tiger.Position (Range (..), (<->))
import Language.Tiger.Semantic.Error
  ( arityMismatch, fieldMismatch, inequalityUnsupportedTypes, notAFunction
  , typeMismatch, undefinedSymbol, unknownField, unsupportedFirstClassFunction
  )
import Language.Tiger.Semantic.Types (Symbol (..), Type (..))
import Language.Tiger.Util (mapAccumLM)

-- * Symbols

data EnvEntry
  = VarEntry Type
  | FunEntry (Vector Type) Type

symbol :: Name -> Tc Symbol
symbol name = do
  TcState{hashtable, nextsym} <- get
  case HashMap.lookup name hashtable of
    Nothing -> do
      put TcState
        { hashtable = HashMap.insertWith (\_ -> (nextsym <|)) name (nextsym :| []) hashtable
        , nextsym = nextsym + 1
        }
      pure $ Symbol name nextsym
    Just is -> pure $ Symbol name $ NE.head is

type Table a = IntMap a

enter :: Symbol -> a -> Table a -> Table a
enter (Symbol _ n) = IntMap.insert n

look :: Symbol -> Table a -> Maybe a
look (Symbol _ n) = IntMap.lookup n

-- * Semantic analysis

data Translate = Translate

data ExpTy = ExpTy
  { exp :: Translate
  , ty :: Type
  }

data TcEnv = TcEnv
  { venv :: Table EnvEntry
  , tenv :: Table Type
  }

instance Semigroup TcEnv where
  TcEnv venv1 tenv1 <> TcEnv venv2 tenv2 =
    -- Right-biased union
    TcEnv (venv2 <> venv1) (tenv2 <> tenv1)

instance Monoid TcEnv where
  mempty = initTcEnv

initTcEnv :: TcEnv
initTcEnv = TcEnv
  { venv = IntMap.fromDistinctAscList $ zip [0..]
    [ fun [String] Unit  -- print
    , fun [] Unit  -- flush
    , fun [] String  -- getchar
    , fun [String] Int  -- ord
    , fun [Int] String  -- chr
    , fun [String] Int  -- size
    , fun [String, Int, Int] String  -- substring
    , fun [String, String] String  -- concat
    , fun [Int] Int  -- not
    , fun [Int] Unit  -- exit
    ]
  , tenv = IntMap.fromDistinctAscList [(10, Int), (11, String)]
  }
  where
    fun tys ret = FunEntry (V.fromList tys) ret

data TcState = TcState
  { hashtable :: HashMap Name (NonEmpty Int)
  , nextsym :: Int
  }

initTcState :: TcState
initTcState = TcState
  { hashtable = HashMap.fromList $ zip
    (Language.Tiger.AST.Name <$> vars <> tys)
    (pure <$> [0..])
  , nextsym = 12
  }
  where
    -- | Built-in functions
    vars :: [ByteString]
    vars =
      [ "print", "flush", "getchar", "ord", "chr", "size", "substring", "concat"
      , "not", "exit"
      ]

    -- | Built-in types
    tys :: [ByteString]
    tys = ["int", "string"]

newtype Tc a = Tc
  { runTc :: ReaderT TcEnv (StateT TcState IO) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader TcEnv, MonadState TcState)

data OpType
  = Arithmetic
  | Equality
  | Inequality
  | Boolean

opType :: BinOp a -> OpType
opType = \case
  Divide _ -> Arithmetic
  Times  _ -> Arithmetic
  Minus  _ -> Arithmetic
  Plus   _ -> Arithmetic
  Eq     _ -> Equality
  Neq    _ -> Equality
  Lt     _ -> Inequality
  Gt     _ -> Inequality
  Ge     _ -> Inequality
  Le     _ -> Inequality
  Conj   _ -> Boolean
  Disj   _ -> Boolean

checkTypes :: Range -> Type -> Type -> Tc ()
checkTypes range expected actual
  | expected == actual = pure ()
  | otherwise          = typeMismatch expected actual range

checkFields :: Range -> (Symbol, Type) -> (Symbol, Type) -> Tc ()
checkFields range expected actual
  | expected == actual = pure ()
  | otherwise          = fieldMismatch expected actual range

lookSymbol :: (TcEnv -> Table a) -> Name -> Range -> Tc a
lookSymbol selector name range = do
  env <- asks selector
  sym <- symbol name
  maybe (undefinedSymbol name range) pure (look sym env)

varSymbol :: Name -> Range -> Tc EnvEntry
varSymbol = lookSymbol venv

typeSymbol :: Name -> Range -> Tc Type
typeSymbol = lookSymbol tenv

ioToTc :: IO a -> Tc a
ioToTc = Tc . lift . lift

transVar :: LValue Range -> Tc ExpTy
transVar (LValue _range (Id idPos idName) accessorChain) = do
  env <- asks venv

  idSym <- symbol idName
  ty <- case look idSym env of
    Nothing -> undefinedSymbol idName idPos
    Just (VarEntry tyId) ->
      flip execStateT tyId $ for_ accessorChain $ \case
        AccessorRecField accessorPos (Id fieldPos fieldName) ->
          case tyId of
            Record _ fields -> case V.find ((== fieldName) . name . fst) fields of
              Nothing -> lift $ undefinedSymbol fieldName (idPos <-> accessorPos)
              Just (_, tyField) -> put tyField
            _ -> unknownField fieldName tyId fieldPos
        AccessorArraySub accessorPos exp -> do
          ExpTy{ty = tyExp} <- lift $ transExp exp
          lift $ checkTypes accessorPos Int tyExp
          case tyId of
            Array _ ty -> put ty
            _ -> do
              unique <- lift $ ioToTc newUnique
              typeMismatch (Array unique tyId) tyId (idPos <-> accessorPos)
    Just (FunEntry _params _result) ->
      unsupportedFirstClassFunction idName idPos

  pure ExpTy{exp = Translate, ty = ty}

transExp :: Exp Range -> Tc ExpTy
transExp = \case
  ELValue _ lvalue -> transVar lvalue
  ENil _ -> pure ExpTy{exp = Translate, ty = Nil}
  ESeq _ exps -> transSeq exps
  EUnit _ -> pure ExpTy{exp = Translate, ty = Unit}
  ELit _ lit -> pure case lit of
    LInt _ _ -> ExpTy{exp = Translate, ty = Int}
    LString _ _ -> ExpTy{exp = Translate, ty = String}
  ENeg range exp -> do
    ExpTy{ty = ty} <- transExp exp
    checkTypes range Int ty
    pure ExpTy{exp = Translate, ty = Int}
  ECall range (Id idRange idName) args -> do
    tyArgs <- traverse (fmap ty . transExp) args
    varSymbol idName idRange >>= \case
      VarEntry typ -> notAFunction idName typ idRange
      FunEntry argTypes retType -> do
        when (V.length argTypes /= V.length tyArgs) $
          arityMismatch idName (V.length argTypes) (V.length tyArgs) range
        V.zipWithM_ (checkTypes range) argTypes tyArgs
        pure ExpTy{exp = Translate, ty = retType}
  EBinOp _ op left right -> do
    ExpTy{ty = tyLeft} <- transExp left
    ExpTy{ty = tyRight} <- transExp right
    let
      rangeLeft = getMeta left
      rangeRight = getMeta right
    case opType op of
      Arithmetic -> do
        checkTypes rangeLeft Int tyLeft
        checkTypes rangeRight Int tyRight
      Equality ->
        checkTypes (getMeta right) tyLeft tyRight
      Inequality ->
        case (tyLeft, tyRight) of
          (Int, Int) -> pure ()
          (String, String) -> pure ()
          (Int, _) -> typeMismatch Int tyRight rangeRight
          (String, _) -> typeMismatch String tyRight rangeRight
          (_, _) -> inequalityUnsupportedTypes op tyLeft tyRight (rangeLeft <-> rangeRight)
      Boolean -> do
        checkTypes rangeLeft Int tyLeft
        checkTypes rangeRight Int tyRight
    pure ExpTy{exp = Translate, ty = Int}
  ERecord range (TypeId tyPos tyName) recFields -> do
    -- Type-check each field of the record, and return each field name with the
    -- inferred type. Will allow us to check that it matches the record declared
    -- with `tyName`.
    tyFields <- for recFields \(RecField _ (Id _ fieldName) exp) -> do
      fieldSym <- symbol fieldName
      ExpTy{ty = tyExp} <- transExp exp
      pure (fieldSym, tyExp)
    typeSymbol tyName tyPos >>= \case
      recTy@(Record unique fields) -> do
        when (V.length fields /= V.length tyFields) $
          typeMismatch recTy (Record unique tyFields) tyPos
        V.zipWithM_ (checkFields range) fields tyFields
        pure ExpTy{exp = Translate, ty = recTy}
      other -> do
        unique <- ioToTc newUnique
        typeMismatch (Record unique tyFields) other tyPos
  EArray _ (TypeId tyPos tyName) numElements initValue -> do
    ExpTy{ty = tyNum} <- transExp numElements
    checkTypes (getMeta numElements) Int tyNum
    ExpTy{ty = tyInit} <- transExp initValue
    ty <- typeSymbol tyName tyPos
    innerTy <- case ty of
      Array _ innerTy -> pure innerTy
      _ -> do
        unique <- ioToTc newUnique
        typeMismatch (Array unique tyInit) ty tyPos
    checkTypes (getMeta initValue) innerTy tyInit
    pure ExpTy{exp = Translate, ty = ty}
  EAssign range lvalue exp -> do
    ExpTy{ty = tyLValue} <- transVar lvalue
    ExpTy{ty = tyExp} <- transExp exp
    checkTypes range tyLValue tyExp
    pure ExpTy{exp = Translate, ty = Unit}
  EIfThenElse range cond then' else' -> do
    ExpTy{ty = tyCond} <- transExp cond
    checkTypes (getMeta cond) Int tyCond
    ExpTy{ty = tyThen'} <- transExp then'
    ExpTy{ty = tyElse'} <- transExp else'
    checkTypes range tyThen' tyElse'
    pure ExpTy{exp = Translate, ty = tyThen'}
  EIfThen _ cond then' -> do
    ExpTy{ty = tyCond} <- transExp cond
    checkTypes (getMeta cond) Int tyCond
    ExpTy{ty = tyThen'} <- transExp then'
    checkTypes (getMeta then') Unit tyThen'
    pure ExpTy{exp = Translate, ty = Unit}
  EWhile _ cond body -> do
    ExpTy{ty = tyCond} <- transExp cond
    checkTypes (getMeta cond) Int tyCond
    ExpTy{ty = tyBody} <- transExp body
    checkTypes (getMeta body) Unit tyBody
    pure ExpTy{exp = Translate, ty = Unit}
  EFor _ (Id _ idName) lower upper body -> do
    ExpTy{ty = tyLower} <- transExp lower
    checkTypes (getMeta lower) Int tyLower
    ExpTy{ty = tyUpper} <- transExp upper
    checkTypes (getMeta upper) Int tyUpper

    sym <- symbol idName
    ExpTy{ty = tyBody} <- local (\env -> env{venv = enter sym (VarEntry Int) (venv env)}) $
      -- TODO: make id immutable
      transExp body
    checkTypes (getMeta body) Unit tyBody

    pure ExpTy{exp = Translate, ty = Unit}
  EBreak _ -> do
    -- TODO: check if in for or while body
    pure ExpTy{exp = Translate, ty = Unit}
  ELet _ decs exps -> do
    let
      accumEnv env dec = do
        env' <- local (<> env) $ transDec dec
        -- Left: new state, which is the old env plus the new env
        -- Right: newly-seen env
        pure (env' <> env, env')
    initEnv <- ask
    envs <- mapAccumLM accumEnv initEnv decs
    -- As per the Tiger specification, we want newer bindings to override older
    -- ones. Thus, we want the old environment to the left, as `<>` is right-biased
    -- for environments.
    tys <- local (<> fold envs) $ traverse transExp exps
    pure $ maybe ExpTy{exp = Translate, ty = Unit} snd $ V.unsnoc tys
  EPar _ exp -> transExp exp

transSeq :: Seq Range -> Tc ExpTy
transSeq = \case
  SeqSpine _ exp exps -> do
    _ <- transExp exp
    transSeq exps
  SeqNil _ exp exp' -> do
    _ <- transExp exp
    transExp exp'

transDec :: Dec Range -> Tc TcEnv
transDec = \case
  TyDec _pos (TypeId _ name) ty -> do
    -- TODO: recursive types
    env <- ask
    ty' <- transTy ty
    sym <- symbol name
    pure env{tenv = enter sym ty' (tenv env)}
  VarDec _pos (Id _ idName) body -> do
    env <- ask
    ExpTy{ty = tyBody} <- transExp body
    sym <- symbol idName
    pure env{venv = enter sym (VarEntry tyBody) (venv env)}
  FunDec _pos (Id _ name) params resultM body -> do
    -- TODO: recursive functions
    env <- ask

    -- Check the result of the function body
    (resultPos, resultTy) <- case resultM of
      Nothing -> pure (getMeta body, Unit)
      Just (TypeId resultPos resultName) -> (resultPos, ) <$> typeSymbol resultName resultPos

    params' <- for params \(TypeField _ (Id _ fieldName) (TypeId tyPos tyName)) -> do
      tySym <- symbol tyName
      fieldSym <- symbol fieldName
      maybe (undefinedSymbol tyName tyPos) (pure . (fieldSym, )) (look tySym (tenv env))
    nameSym <- symbol name
    let
      venv' = enter nameSym (FunEntry (snd <$> params') resultTy) (venv env)
      enterParam offset ty = enter offset (VarEntry ty)
      venv'' = V.foldl' (flip (uncurry enterParam)) venv' params'
    -- Typecheck the body with new params
    ExpTy{ty = tyBody} <- local (const env{venv = venv''}) (transExp body)
    checkTypes resultPos resultTy tyBody
    -- Do not return the new parameters, only the function type
    pure env{venv = venv'}

transTy :: Ty Range -> Tc Type
transTy = \case
  TyId _ (TypeId range name) ->
    typeSymbol name range
  TyFields _ fields -> do
    unique <- ioToTc newUnique
    fieldTys <- for fields \(TypeField _ (Id _ fieldName) (TypeId tyRange tyName)) -> do
      fieldSym <- symbol fieldName
      ty <- typeSymbol tyName tyRange
      pure (fieldSym, ty)
    pure $ Record unique fieldTys
  TyArray _ (TypeId range name) -> do
    unique <- ioToTc newUnique
    ty <- typeSymbol name range
    pure $ Array unique ty

transProg :: Exp Range -> Tc ()
transProg = void . transExp
