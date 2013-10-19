module TestRope where

import Test.HUnit
import Test.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty (TestTree, testGroup)

import Data.Monoid
import qualified Data.Rope as R

newline :: R.Rope
newline = R.singleton '\n'

tests :: TestTree
tests = testGroup "Rope"
    [ testProperty "conversion" $
        \s -> R.toString (R.fromString s) == s
    , testProperty "reverse" $
        \s -> R.fromString (reverse s) == R.reverse (R.fromString s)
    , testProperty "null" $
        \s -> null s == R.null (R.fromString s)
    , testProperty "take" $
        \s i -> i >= 0 ==> R.fromString (take i s) == R.take i (R.fromString s)
    , testProperty "drop" $
        \s i -> i >= 0 ==> R.fromString (drop i s) == R.drop i (R.fromString s)
    , testProperty "length" $
        \s -> length s == R.length (R.fromString s)
    , testProperty "append" $
        \s t -> R.fromString (s ++ t) == R.append (R.fromString s) (R.fromString t)
    , testProperty "concat" $
        \ss -> R.fromString (concat ss) == R.concat (map R.fromString ss)
    , testProperty "countNewLines" $
        \s -> length (filter (== '\n') s) == R.countNewLines (R.fromString s)
    , testProperty "splitAt" $
        \s i -> i >= 0 ==> R.splitAt i (R.fromString s) ==
            (let (x, y) = splitAt i s in (R.fromString x, R.fromString y))
    , testProperty "splitAtLine 0" $
        \s -> let r = R.fromString s in R.splitAtLine 0 r == (R.empty, r)
    , testProperty "splitAtLine 1" $
        \s t -> '\n' `notElem` s ==>
            let r = R.fromString s
                q = R.fromString t
            in R.splitAtLine 1 (r <> newline <> q) == (r <> newline, q)
    , testProperty "splitAtLine i" $
        \s i ->
            let rq = R.fromString s
                (r, q) = R.splitAtLine i rq
            in rq == r <> q
    , testCase "splitAtLine 1 'a\\nb'" $
        R.splitAtLine 1 (R.fromString "a\nb") @=? (R.fromString "a\n", R.fromString "b")
    , testCase "splitAtLine 1 '\\n'" $
        R.splitAtLine 1 (R.fromString "\n") @=? (R.fromString "\n", R.empty)
    , testCase "splitAtLine 1 '\\n\\n'" $
        R.splitAtLine 1 (R.fromString "\n\n") @=? (newline, newline)
    ]
