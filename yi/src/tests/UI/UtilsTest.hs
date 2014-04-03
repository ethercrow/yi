{-# LANGUAGE TemplateHaskell #-}
module UI.UtilsTest where

import Test.HUnit
import Test.QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Yi.UI.Utils

tests :: TestTree
tests = $(testGroupGenerator)

case_arrangeItems_1 =
    arrangeItems ["foo", "bar", "baz"] 11 1 @?= ["foo bar baz"]

case_arrangeItems_2 =
    arrangeItems ["foo", "bar", "baz"] 80 2 @?= ["foo bar baz"]

case_arrangeItems_3 =
    arrangeItems ["foo", "bar", "baz"] 11 2 @?= ["foo bar baz"]

case_arrangeItems_4 =
    arrangeItems ["foo", "bar", "baz"] 12 12 @?= ["foo bar baz"]

case_arrangeItems_5 =
    arrangeItems ["foo", "bar", "baz"] 7 2 @?= ["foo baz", "bar"]

case_arrangeItems_6 =
    arrangeItems ["foo", "bar", "baz", "quux"] 4 4 @?= ["foo ", "bar ", "baz ", "quux"]

case_arrangeItems_7 =
    arrangeItems ["foo", "bar", "baz", "quux"] 9 2 @?= ["foo baz ", "bar quux"]

case_arrangeItems'_1 =
    arrangeItems' ["foo", "bar", "baz"] 11 1 @?= (3, ["foo bar baz"])

case_arrangeItems'_2 =
    arrangeItems' ["foo", "bar", "baz"] 80 2 @?= (3, ["foo baz", "bar"])

case_arrangeItems'_3 =
    arrangeItems' ["foo", "bar", "baz"] 11 2 @?= (3, ["foo baz", "bar"])

case_arrangeItems'_4 =
    arrangeItems' ["foo", "bar", "baz"] 12 5 @?= (3, ["foo", "bar", "baz"])

case_arrangeItems'_5 =
    arrangeItems' ["foo", "bar", "baz"] 7 2 @?= (3, ["foo baz", "bar"])

case_arrangeItems'_6 =
    arrangeItems' ["foo", "bar", "baz", "quux"] 4 4 @?= (4, ["foo ", "bar ", "baz ", "quux"])

case_arrangeItems'_7 =
    arrangeItems' ["foo", "bar", "baz"] 7 1 @?= (2, ["foo bar"])

case_arrangeItems'_8 =
    arrangeItems' ["foo", "bar", "baz"] 3 2 @?= (2, ["foo", "bar"])

