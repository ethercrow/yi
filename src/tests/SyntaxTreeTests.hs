{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}


module SyntaxTreeTests (tests) where

import Control.Applicative
import Data.Foldable
import Data.Monoid

import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property (unProperty)

import Yi.Buffer
import Yi.Lexer.Alex (Posn (Posn, posnLine, posnOfs), Tok (Tok, tokPosn), tokBegin, tokEnd)
import Yi.Region (Region (regionEnd, regionStart), includedRegion, mkRegion)
import Yi.Syntax.Tree

tests = $(testGroupGenerator)

nodeRegion :: IsTree tree => Node (tree (Tok a)) -> Region
nodeRegion n = subtreeRegion t
    where Just t = walkDown n

data Test a = Empty | Leaf a | Bin (Test a) (Test a) deriving (Show, Eq, Foldable)

instance IsTree Test where
    uniplate (Bin l r) = ([l,r],\[l',r'] -> Bin l' r')
    uniplate t = ([],\[] -> t)
    emptyNode = Empty

type TT = Tok ()

instance Arbitrary (Test TT) where
    arbitrary = sized $ \size -> do
      arbitraryFromList [1..size+1]
    shrink (Leaf _) = []
    shrink (Bin l r) = [l,r] <>  (Bin <$> shrink l <*> pure r) <>  (Bin <$> pure l <*> shrink r)

tAt :: Point -> TT
tAt idx =  Tok () 1 (Posn (idx * 2) 0 0)

arbitraryFromList :: [Int] -> Gen (Test TT)
arbitraryFromList [] = error "arbitraryFromList expects non empty lists"
arbitraryFromList [x] = pure (Leaf (tAt (fromIntegral x)))
arbitraryFromList xs = do
  m <- choose (1,length xs - 1)
  let (l,r) = splitAt m xs
  Bin <$> arbitraryFromList l <*> arbitraryFromList r

newtype NTTT = N (Node (Test TT)) deriving Show

instance Arbitrary NTTT where
    arbitrary = do
      t <- arbitrary
      p <- arbitraryPath t
      return $ N (p,t)

arbitraryPath :: Test t -> Gen Path
arbitraryPath (Leaf _) = return []
arbitraryPath (Bin l r) = do
  c <- choose (0,1)
  let Just n' = index c [l,r]
  (c :) <$> arbitraryPath n'

regionInside :: Region -> Gen Region
regionInside r = do
  b :: Int <- choose (fromIntegral $ regionStart r, fromIntegral $ regionEnd r)
  e :: Int <- choose (b, fromIntegral $ regionEnd r)
  return $ mkRegion (fromIntegral b) (fromIntegral e)

pointInside :: Region -> Gen Point
pointInside r = do
  p :: Int <- choose (fromIntegral $ regionStart r, fromIntegral $ regionEnd r)
  return (fromIntegral p)

prop_fromLeafAfterToFinal :: NTTT -> Property
prop_fromLeafAfterToFinal (N n) = let
    fullRegion = subtreeRegion $ snd n
 in forAll (pointInside fullRegion) $ \p -> do
   let final@(_, (_, finalSubtree)) = fromLeafAfterToFinal p n
       finalRegion = subtreeRegion finalSubtree
       initialRegion = nodeRegion n

   whenFail (do putStrLn $ "final = " <> show final
                putStrLn $ "final reg = " <> show finalRegion
                putStrLn $ "initialReg = " <> show initialRegion
                putStrLn $ "p = " <> show p
            )
     ((regionStart finalRegion <= p) && (initialRegion `includedRegion` finalRegion))

prop_allLeavesAfter :: NTTT -> Property
prop_allLeavesAfter (N n@(xs,t)) = property $ do
  let after = allLeavesRelative afterChild n
  (xs',t') <- elements after
  let t'' = walkDown (xs',t)
  unProperty $ whenFail (do
      putStrLn $ "t' = " <> show t'
      putStrLn $ "t'' = " <> show t''
      putStrLn $ "xs' = " <> show xs'
    ) (Just t' == t'' && xs <= xs')

prop_allLeavesBefore :: NTTT -> Property
prop_allLeavesBefore (N n@(xs,t)) = property $ do
  let after = allLeavesRelative beforeChild n
  (xs',t') <- elements after
  let t'' = walkDown (xs',t)
  unProperty $ whenFail (do
      putStrLn $ "t' = " <> show t'
      putStrLn $ "t'' = " <> show t''
      putStrLn $ "xs' = " <> show xs'
    ) (Just t' == t'' && xs' <= xs)

prop_fromNodeToLeafAfter :: NTTT -> Property
prop_fromNodeToLeafAfter (N n) = forAll (pointInside (subtreeRegion $ snd n)) $ \p -> do
   let after = fromLeafToLeafAfter p n
       afterRegion = nodeRegion after
   whenFail (do putStrLn $ "after = " <> show after
                putStrLn $ "after reg = " <> show afterRegion
            )
     (regionStart afterRegion >= p)

prop_fromNodeToFinal :: NTTT -> Property
prop_fromNodeToFinal  (N t) = forAll (regionInside (subtreeRegion $ snd t)) $ \r -> do
   let final@(_, finalSubtree) = fromNodeToFinal r t
       finalRegion = subtreeRegion finalSubtree
   whenFail (do putStrLn $ "final = " <> show final
                putStrLn $ "final reg = " <> show finalRegion
                putStrLn $ "leaf after = " <> show (fromLeafToLeafAfter (regionEnd r) t)
            ) $ do
     r `includedRegion` finalRegion