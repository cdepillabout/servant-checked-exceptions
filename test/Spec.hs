module Main where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)

import Spec.ApiSpec (apiTests)
import Spec.HelperFuncSpec (helperFuncTests)
import Spec.ServerSpec (serverTestsIO)
import Spec.TestDirLocation (testDir)

main :: IO ()
main = do
  createDirectoryIfMissing False (testDir </> "empty-dir")
  tests <- testsIO
  defaultMain tests

testsIO :: IO TestTree
testsIO = do
  serverTests <- serverTestsIO
  pure $ testGroup "tests" [helperFuncTests, apiTests, serverTests]
