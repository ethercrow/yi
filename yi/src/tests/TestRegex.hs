module TestRegex (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Data.List (intercalate)
import qualified Data.Rope as R

import Yi.Regex
import Yi.Buffer
import Yi.Region

test_makeSearchExp :: TestTree
test_makeSearchExp = testGroup "makeSearchExp" [
    testCase "literal" $ do
        let literalRegex = makeSearchExp [QuoteRegex] "foobar"
        case literalRegex of
            Right (SearchExp i _ opts) -> do
                i @=? "foobar"
                opts @=? [QuoteRegex]
            Left msg -> assertFailure msg
  , testCase "dot star" $ do
        let dotStar = makeSearchExp [] ".*"
        case dotStar of
            Right (SearchExp i _ opts) -> do
                i @=? ".*"
                opts @=? []
            Left msg -> assertFailure msg
  , testCase "empty regex" $ do
        let r = makeSearchExp [] ""
        case r of
            Right _ -> assertFailure $ "Expected failure but got " ++ show r
            Left _ -> return ()
  , testCase "invalid regex" $ do
        let r = makeSearchExp [] "*"
        case r of
            Right _ -> assertFailure $ "Expected failure but got " ++ show r
            Left _ -> return ()
  ]

buf1 :: FBuffer
buf1 = newB (BufferRef 0) (Left "buf1") $ R.fromString $ intercalate "\n" [
    "lorem ipsum dolor sit amet"
  , "lorem lorem DOLOR"
  ]

test_regexB :: TestTree
test_regexB = testGroup "regexB" [
    testCase "lorem backward" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "lorem"
            regions = runBufferDummyWindow buf1 (botB >> regexB Backward se)
        regions @?= [
            mkRegion 33 38
          , mkRegion 27 32
          , mkRegion 0 5
          ]
  , testCase "lorem forward" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "lorem"
            regions = runBufferDummyWindow buf1 (regexB Forward se)
        regions @?= [
            mkRegion 0 5
          , mkRegion 27 32
          , mkRegion 33 38
          ]
  ]

test_regexRegionB :: TestTree
test_regexRegionB = testGroup "regexRegionB" [
    testCase "lorem" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "lorem"
            regions = runBufferDummyWindow buf1 (regexRegionB se $ mkRegion 0 5)
        regions @?= [mkRegion 0 5]
  , testCase "lorem 2" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "lorem"
            regions = runBufferDummyWindow buf1 (regexRegionB se $ mkRegion 20 33)
        regions @?= [mkRegion 27 32]
  , testCase "lorem 3" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "lorem"
            regions = runBufferDummyWindow buf1 (regexRegionB se $ mkRegion 0 33)
        regions @?= [mkRegion 0 5, mkRegion 27 32]
  , testCase "DOLOR" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "DOLOR"
            regions = runBufferDummyWindow buf1 (regexRegionB se $ mkRegion 30 44)
        regions @?= [mkRegion 39 44]
  , testCase "DOLOR case mismatch" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "DOLOR"
            regions = runBufferDummyWindow buf1 (regexRegionB se $ mkRegion 0 25)
        regions @?= []
  ]

test_backwards_regexRegionB :: TestTree
test_backwards_regexRegionB = testGroup "backwards_regexRegionB" [
    testCase "lorem" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "lorem"
            regions = runBufferDummyWindow buf1 (regexRegionB se $ mkRegion 5 0)
        regions @?= [mkRegion 0 5]
  , testCase "lorem 2" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "lorem"
            regions = runBufferDummyWindow buf1 (regexRegionB se $ mkRegion 33 20)
        regions @?= [mkRegion 27 32]
  , testCase "lorem 3" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "lorem"
            regions = runBufferDummyWindow buf1 (regexRegionB se $ mkRegion 33 0)
        regions @?= [mkRegion 0 5, mkRegion 27 32]
  , testCase "DOLOR" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "DOLOR"
            regions = runBufferDummyWindow buf1 (regexRegionB se $ mkRegion 44 30)
        regions @?= [mkRegion 39 44]
  , testCase "DOLOR case mismatch" $ do
        let (Right se) = makeSearchExp [QuoteRegex] "DOLOR"
            regions = runBufferDummyWindow buf1 (regexRegionB se $ mkRegion 25 0)
        regions @?= []
  ]

tests :: TestTree
tests = testGroup "Regex" [
    test_makeSearchExp
  , test_regexB
  , test_regexRegionB
  , test_backwards_regexRegionB
  ]