module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified TestVim
import qualified TestRope

main :: IO ()
main = do
    tests <- TestVim.getTests
    let ropeTests = TestRope.tests
    defaultMain $ testGroup "Tests" [ropeTests]