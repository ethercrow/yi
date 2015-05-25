module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Vim.TestPureBufferManipulations as VimBuffer
import qualified Vim.TestPureEditorManipulations as VimEditor
import qualified Vim.TestExCommandParsers as VimExCommand
import qualified SyntaxTreeTests

main :: IO ()
main = do
    tests  <- VimBuffer.getTests
    defaultMain $ testGroup "Tests" [
        tests
      , VimEditor.tests
      , VimExCommand.tests
      , SyntaxTreeTests.tests
      ]
