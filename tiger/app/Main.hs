module Main (main) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.ByteString.Lazy.Char8 qualified as BS

import Language.Tiger.Parser (parseTiger, runAlex)
import Language.Tiger.Semantic.TypeChecker (initTcEnv, initTcState, runTc, transProg)

main :: IO ()
main = do
  file <- BS.readFile "test/programs/parser/positive/merge.tig"
  let parsed = runAlex file parseTiger
  either putStrLn (flip evalStateT initTcState . flip runReaderT initTcEnv . runTc . transProg) parsed
