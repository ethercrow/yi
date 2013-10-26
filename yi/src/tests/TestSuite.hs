module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified TestVim
import qualified TestRope
import qualified TestRegex

main :: IO ()
main = do
    tests <- TestVim.getTests
    let ropeTests = TestRope.tests
        regexTests = TestRegex.tests
    defaultMain $ testGroup "Tests" [tests, ropeTests, regexTests]