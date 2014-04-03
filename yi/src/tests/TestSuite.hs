module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Vim2.TestPureBufferManipulations as Vim2Buffer
import qualified Vim2.TestPureEditorManipulations as Vim2Editor
import qualified Vim2.TestExCommandParsers as Vim2ExCommand
import qualified UI.UtilsTest
import qualified RopeTest

main :: IO ()
main = do
    tests  <- Vim2Buffer.getTests
    defaultMain $ testGroup "Tests" [
        tests
      , RopeTest.tests
      , UI.UtilsTest.tests
      , Vim2Editor.tests
      , Vim2ExCommand.tests
      ]
