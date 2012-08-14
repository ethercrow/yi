
module Main where

import Test.HUnit
import Control.Monad (forM_)

import Yi.Window

testWindow :: Int -> Window
testWindow idx = (dummyWindow 0) { jumpList = JumpList [("foo", 1), ("bar", 2), ("baz", 3)] idx } 

test0 :: Test
test0 = TestCase $ do
    let w = testWindow 0
    let w' = jumpBack ("xyzzy", 4) 1 $ jumpForward 1 w
    let expected = JumpList [("foo", 1), ("bar", 2), ("xyzzy", 4)] 1
    assertEqual "" expected (jumpList w')

test1 :: Test
test1 = TestCase $ do
    let w = testWindow 2
    let ws = map (\count -> (jumpForward count $ jumpBack ("baz", 3) count w,
                             "trial #" ++ show count))
                 [1 .. 2]
    forM_ ws $ \(w', m) ->
        assertEqual m (jumpList w) (jumpList w')

test2 :: Test
test2 = TestCase $ do
    let w = testWindow 2
    assertEqual "" (jumpList w) (jumpList (jumpForward 1 w))

test3 :: Test
test3 = TestCase $ do
    let w = testWindow 0
    assertEqual "" (jumpList w) (jumpList (jumpBack ("xyzzy", 4) 1 w))

test4 :: Test
test4 = TestCase $ do
    let w = testWindow 2
    let ws = map (\count -> (jumpForward count $ jumpBack ("xyzzy", 4) count w,
                             "trial #" ++ show count))
                 [1 .. 2]
    let expected = JumpList [("foo", 1), ("bar", 2), ("baz", 3), ("xyzzy", 4)] 3
    forM_ ws $ \(w', m) ->
        assertEqual m expected (jumpList w')

test5 :: Test
test5 = TestCase $ do
    let w = testWindow 1
    assertEqual "" (jumpList w) (jumpList (jumpForward 0 w))

test6 :: Test
test6 = TestCase $ do
    let w = testWindow 1
    assertEqual "" (jumpList w) (jumpList (jumpBack ("xyzzy", 4) 0 w))

test7 :: Test
test7 = TestCase $ do
    let w = testWindow 1
    assertEqual "" (jumpList w) (jumpList (jumpForward 100 w))

test8 :: Test
test8 = TestCase $ do
    let w = testWindow 1
    assertEqual "" (jumpList w) (jumpList (jumpBack ("xyzzy", 4) 100 w))

test9 :: Test
test9 = TestCase $ do
    let w = testWindow 0
    let w' = jumpBack ("xyzzy", 4) 2 $ jumpForward 2 w
    let expected = JumpList [("foo", 1), ("bar", 2), ("baz", 3), ("xyzzy", 4)] 1
    assertEqual "" expected (jumpList w')

tests :: Test
tests = TestList [
    TestLabel "test0" test0,
    TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4,
    TestLabel "test5" test5,
    TestLabel "test6" test6,
    TestLabel "test7" test7,
    TestLabel "test8" test8,
    TestLabel "test9" test9
    ]

main :: IO ()
main = do
    _ <- runTestTT tests
    return ()
