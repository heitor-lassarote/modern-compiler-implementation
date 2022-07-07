module Language.Tiger.Semantic.TypeChecker
  ( Tc, runTc, TcState, initTcState, TcReader, initTcReader, TcEnv
  , transProg
  ) where

import Prelude hiding (exp)

import Control.Monad (unless, void, when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local)
import Control.Monad.State (MonadState, StateT, execStateT, get, put)
import Control.Monad.Trans (lift)
import Data.Bool (bool)
import Data.Foldable (fold, for_, traverse_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Semigroup (Arg (..))
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
  ( arityMismatch, assignedToForVar, cyclicTypeAliases, duplicatedNames, fieldMismatch
  , illegalBreak, illegalNil, inequalityUnsupportedTypes, notAFunction, panic, typeMismatch
  , undefinedSymbol, unknownField, unsupportedFirstClassFunction
  )
import Language.Tiger.Semantic.Types (Symbol (..), Type (..))
import Language.Tiger.Semantic.Types qualified as Ty
import Language.Tiger.Util (findCycles, findDuplicatesOn, mapAccumLM)

-- * Symbols

data Assignable = Can'tAssign | CanAssign

data EnvEntry
  = VarEntry Assignable Type
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

data TcReader = TcReader
  { tcEnv :: TcEnv
  , inLoop :: Bool
  }

initTcReader :: TcReader
initTcReader = TcReader
  { tcEnv = initTcEnv
  , inLoop = False
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
  mempty = TcEnv mempty mempty

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

withEnv :: (TcEnv -> TcEnv) -> Tc a -> Tc a
withEnv f = local (\r -> r{tcEnv = f $ tcEnv r})

data TcState = TcState
  { hashtable :: HashMap Name (NonEmpty Int)
  , nextsym :: Int
  }

initTcState :: TcState
initTcState = TcState
  { hashtable = HashMap.fromList $ zip (vars <> tys) (pure <$> [0..])
  , nextsym = 12
  }
  where
    -- | Built-in functions
    vars :: [Name]
    vars =
      [ "print", "flush", "getchar", "ord", "chr", "size", "substring", "concat"
      , "not", "exit"
      ]

    -- | Built-in types
    tys :: [Name]
    tys = ["int", "string"]

newtype Tc a = Tc
  { runTc :: ReaderT TcReader (StateT TcState IO) a
  } deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader TcReader, MonadState TcState)

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

canonicalizeType :: Range -> Type -> Tc Type
canonicalizeType range (Ty.Name _ ref) = do
  t <- ioToTc $ readIORef ref
  maybe (panic $ "Name without type was dereferenced at " <> show range) (canonicalizeType range) t
canonicalizeType _ t = pure t

checkTypes :: Range -> Type -> Type -> Tc Type
checkTypes range expected actual = do
  expected' <- canonicalizeType range expected
  actual'   <- canonicalizeType range actual
  case (expected', actual') of
    (Nil, Nil) -> illegalNil range
    (Nil, Record{}) -> pure actual'
    (Record{}, Nil) -> pure expected'
    (Nil, _) -> illegalNil range
    (_, Nil) -> illegalNil range
    _ | expected' == actual' -> pure expected'
      | otherwise -> typeMismatch expected' actual' range

checkFields :: Range -> (Symbol, Type) -> (Symbol, Type) -> Tc ()
checkFields range (expectedS, expectedT) (actualS, actualT) = do
  expectedT' <- canonicalizeType range expectedT
  actualT'   <- canonicalizeType range actualT
  let expected' = (expectedS, expectedT')
  let actual'   = (actualS,   actualT')
  bool (fieldMismatch expected' actual' range) (pure ()) (expected' == actual')

lookSymbol :: (TcEnv -> Table a) -> Name -> Range -> Tc a
lookSymbol selector name range = do
  env <- asks $ selector . tcEnv
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
  env <- asks $ venv . tcEnv

  idSym <- symbol idName
  ty <- case look idSym env of
    Nothing -> undefinedSymbol idName idPos
    Just (VarEntry _ tyId) ->
      flip execStateT tyId $ for_ accessorChain $ \case
        AccessorRecField accessorPos (Id fieldPos fieldName) ->
          case tyId of
            Record _ fields -> case V.find ((== fieldName) . name . fst) fields of
              Nothing -> lift $ undefinedSymbol fieldName (idPos <-> accessorPos)
              Just (_, tyField) -> put tyField
            _ -> unknownField fieldName tyId fieldPos
        AccessorArraySub accessorPos exp -> do
          ExpTy{ty = tyExp} <- lift $ transExp exp
          lift $ void $ checkTypes accessorPos Int tyExp
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
    void $ checkTypes range Int ty
    pure ExpTy{exp = Translate, ty = Int}
  ECall range (Id idRange idName) args -> do
    tyArgs <- traverse (fmap ty . transExp) args
    varSymbol idName idRange >>= \case
      VarEntry _ typ -> notAFunction idName typ idRange
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
        void $ checkTypes rangeLeft Int tyLeft
        void $ checkTypes rangeRight Int tyRight
      Equality ->
        void $ checkTypes (getMeta right) tyLeft tyRight
      Inequality ->
        case (tyLeft, tyRight) of
          (Int, Int) -> pure ()
          (String, String) -> pure ()
          (Int, _) -> typeMismatch Int tyRight rangeRight
          (String, _) -> typeMismatch String tyRight rangeRight
          (_, _) -> inequalityUnsupportedTypes op tyLeft tyRight (rangeLeft <-> rangeRight)
      Boolean -> do
        void $ checkTypes rangeLeft Int tyLeft
        void $ checkTypes rangeRight Int tyRight
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
    void $ checkTypes (getMeta numElements) Int tyNum
    ExpTy{ty = tyInit} <- transExp initValue
    ty <- typeSymbol tyName tyPos
    innerTy <- case ty of
      Array _ innerTy -> pure innerTy
      _ -> do
        unique <- ioToTc newUnique
        typeMismatch (Array unique tyInit) ty tyPos
    void $ checkTypes (getMeta initValue) innerTy tyInit
    pure ExpTy{exp = Translate, ty}
  EAssign range lvalue@(LValue _ (Id idRange idName) accessorChain) exp -> do
    when (V.null accessorChain) do
      entry <- varSymbol idName idRange
      case entry of
        VarEntry Can'tAssign _ -> assignedToForVar idName range
        VarEntry CanAssign _ -> pure ()
        FunEntry _ _ -> pure ()
    ExpTy{ty = tyLValue} <- transVar lvalue
    ExpTy{ty = tyExp} <- transExp exp
    void $ checkTypes range tyLValue tyExp
    pure ExpTy{exp = Translate, ty = Unit}
  EIfThenElse range cond then' else' -> do
    ExpTy{ty = tyCond} <- transExp cond
    void $ checkTypes (getMeta cond) Int tyCond
    ExpTy{ty = tyThen'} <- transExp then'
    ExpTy{ty = tyElse'} <- transExp else'
    ty <- checkTypes range tyThen' tyElse'
    pure ExpTy{exp = Translate, ty}
  EIfThen _ cond then' -> do
    ExpTy{ty = tyCond} <- transExp cond
    void $ checkTypes (getMeta cond) Int tyCond
    ExpTy{ty = tyThen'} <- transExp then'
    void $ checkTypes (getMeta then') Unit tyThen'
    pure ExpTy{exp = Translate, ty = Unit}
  EWhile _ cond body -> do
    ExpTy{ty = tyCond} <- transExp cond
    void $ checkTypes (getMeta cond) Int tyCond
    ExpTy{ty = tyBody} <- local (\r -> r{inLoop = True}) $ transExp body
    void $ checkTypes (getMeta body) Unit tyBody
    pure ExpTy{exp = Translate, ty = Unit}
  EFor _ (Id _ idName) lower upper body -> do
    ExpTy{ty = tyLower} <- transExp lower
    void $ checkTypes (getMeta lower) Int tyLower
    ExpTy{ty = tyUpper} <- transExp upper
    void $ checkTypes (getMeta upper) Int tyUpper

    sym <- symbol idName
    let
      enterFor r = let env = tcEnv r in r
        { tcEnv = env{venv = enter sym (VarEntry Can'tAssign Int) (venv env)}
        , inLoop = True
        }
    ExpTy{ty = tyBody} <- local enterFor $ transExp body
    void $ checkTypes (getMeta body) Unit tyBody

    pure ExpTy{exp = Translate, ty = Unit}
  EBreak pos -> do
    TcReader{inLoop} <- ask
    unless inLoop $ illegalBreak pos
    pure ExpTy{exp = Translate, ty = Unit}
  ELet _ decs exps -> do
    env <- transDecs decs
    tys <- withEnv (const env) $ traverse transExp exps
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

transDecs :: Vector (Dec Range) -> Tc TcEnv
transDecs decs = withAccumTyEnvs (V.groupBy isDecGroup decs) transDecsGroup
  where
    isDecGroup :: Dec Range -> Dec Range -> Bool
    isDecGroup TyDec{}  TyDec{}  = True
    isDecGroup FunDec{} FunDec{} = True
    isDecGroup _        _        = False

withAccumTyEnvs :: Traversable t => t a -> (a -> Tc TcEnv) -> Tc TcEnv
withAccumTyEnvs input action = do
  initEnv <- asks tcEnv
  fold <$> mapAccumLM accumEnvs initEnv input
  where
    accumEnvs env input' = do
      env' <- withEnv (<> env) $ action input'
      let env'' = env' <> env
      -- accum and state
      pure (env'', env'')

-- | Checks that a group of declarations do not contain duplicated names, and
-- that a group of types do not contain cycles in type aliases.
validateDecsGroup :: Vector (Dec Range) -> Tc ()
validateDecsGroup (V.toList -> xs) = reportDuplicates *> reportTypeCycles
  where
    reportDuplicates, reportTypeCycles :: Tc ()
    reportDuplicates =
      traverse_ duplicatedNames $ NE.nonEmpty $ findDuplicatesOn extractName getMeta xs
    reportTypeCycles =
      traverse_ (cyclicTypeAliases . fmap (fmap (\(Arg tyId tyR) -> (tyId, tyR))))
      $ NE.nonEmpty $ findCycles $ extractTyEdges xs

    extractName :: Dec Range -> Name
    extractName = \case
      TyDec _ (TypeId _ name) _ -> name
      VarDec _ (Id _ name) _ _ -> name
      FunDec _ (Id _ name) _ _ _ -> name

    extractTyEdge :: Dec Range -> Maybe (Arg Name Range, Arg Name Range)
    extractTyEdge = \case
      TyDec _ (TypeId tyR tyId) (TyId _ (TypeId tyR' tyId')) -> Just (Arg tyId tyR, Arg tyId' tyR')
      TyDec{} -> Nothing
      VarDec{} -> Nothing
      FunDec{} -> Nothing

    extractTyEdges :: [Dec Range] -> [(Arg Name Range, Arg Name Range)]
    extractTyEdges = mapMaybe extractTyEdge

transDecsGroup :: Vector (Dec Range) -> Tc TcEnv
transDecsGroup decs = do
  validateDecsGroup decs

  env <- asks tcEnv
  -- Put all the headers in the environment.
  env' <- withAccumTyEnvs decs \case
    TyDec _ (TypeId _ name) _ -> do
      sym <- symbol name
      header <- ioToTc $ Ty.Name sym <$> newIORef Nothing
      pure env{tenv = enter sym header (tenv env)}
    VarDec{} -> pure env
    FunDec _ (Id _ name) params resultM _ -> do
      sym <- symbol name
      retTy <- maybe (pure Unit) (\(TypeId resultRange result) -> typeSymbol result resultRange) resultM
      paramTys <- for params \(TypeField _ _ (TypeId tyRange ty)) -> typeSymbol ty tyRange
      pure env{venv = enter sym (FunEntry paramTys retTy) (venv env)}

  -- Find the types for each thing.
  fold <$> withEnv (<> env') (traverse transDec decs)

transDec :: Dec Range -> Tc TcEnv
transDec = \case
  TyDec _pos (TypeId namePos name) ty -> do
    env <- asks tcEnv
    ty' <- transTy ty
    typeSymbol name namePos >>= \case
      Ty.Name _ tyRef -> ioToTc $ writeIORef tyRef $ Just ty'
      _ -> panic $ "Non-`Name` seen in `transDec` at " <> show namePos

    sym <- symbol name
    pure env{tenv = enter sym ty' (tenv env)}
  VarDec _pos (Id _ idName) tyIdM body -> do
    env <- asks tcEnv
    ExpTy{ty = tyBody} <- transExp body
    ty <- case tyIdM of
      Nothing -> case tyBody of
        Nil -> illegalNil (getMeta body)
        _ -> pure tyBody
      Just (TypeId tyPos tyName) -> do
        tyVar <- typeSymbol tyName tyPos
        checkTypes tyPos tyVar tyBody
    sym <- symbol idName
    pure env{venv = enter sym (VarEntry CanAssign ty) (venv env)}
  FunDec _pos (Id _ name) params resultM body -> do
    env <- asks tcEnv

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
      enterParam offset ty = enter offset (VarEntry CanAssign ty)
      venv'' = V.foldl' (flip (uncurry enterParam)) venv' params'
    -- Typecheck the body with new params
    ExpTy{ty = tyBody} <- withEnv (const env{venv = venv''}) (transExp body)
    void $ checkTypes resultPos resultTy tyBody
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
