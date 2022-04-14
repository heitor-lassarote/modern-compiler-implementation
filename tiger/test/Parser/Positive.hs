module Parser.Positive
  ( test_positive
  ) where

import Data.ByteString.Lazy qualified as BS
import Data.Functor ((<&>))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import Language.Tiger.Lexer (runAlex)
import Language.Tiger.Parser (parseTiger)

testDir :: FilePath
testDir = "test" </> "programs" </> "parser" </> "positive"

testCases :: [FilePath]
testCases =
  [ testDir </> "merge.tig"
  , testDir </> "queens.tig"
  ]

test_positive :: TestTree
test_positive =
  testGroup "Parser: positive tests" $
    testCases <&> \file ->
      testCase file do
        result <- flip runAlex parseTiger <$> BS.readFile file
        either assertFailure (const (pure ())) result
