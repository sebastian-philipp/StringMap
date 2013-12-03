{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
where

import           Prelude                  hiding (lookup, map, mapM, null, succ)
import           Data.StringMap.Strict

import           Control.Arrow         (second)
import           Control.DeepSeq       (($!!))
import           Data.Monoid

import           GHC.AssertNF

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import qualified Test.QuickCheck as Q (arbitrary, Property)
import qualified Test.QuickCheck.Monadic as Q (assert, monadicIO, pick, run, PropertyM)
import           Test.HUnit                           hiding (Test, Testable)

newtype Attr = A [Int]
    deriving (Show)

instance Monoid Attr where
    mempty = mkA []
    mappend (A xs) (A ys) = mkA $! xs ++ ys -- mappend needs to be strict, because the stingList won't evaluate A deep.


type Map = StringMap Attr

-- strict constructor for Attr

mkA :: [Int] -> Attr
mkA xs = A $!! xs

consA :: Int -> Attr -> Attr
consA n a = mkA [n] `mappend` a

default (Int)

main :: IO ()
main = defaultMain
       [
         testCase "isNF" test_isNF
       , testCase "m0" (checkIsNF m0)
       , testCase "m1" (checkIsNF m1)
       , testCase "m2" (checkIsNF m2)
       , testCase "m3" (checkIsNF m3)
       , testCase "m5" (checkIsNF m3)
       , testCase "m6" (checkIsNF m3)
       , testCase "fromList l4" (checkIsNF $ fromList l4)
       , testCase "m2 union m3" (checkIsNF $ m2 `union` m3)
       , testCase "m2 unionWith m2" (checkIsNF $ unionWith mappend m2 m2)
       , testCase "adjust m6" (checkIsNF $ adjust (consA 42) "ab" m6)
       , testCase "adjust m1" (checkIsNF $ adjust (consA 42) "xx" m1)
       , testCase "delete m6" (checkIsNF $ delete "ab" m6)
       , testCase "delete m1" (checkIsNF $ delete "xx" m1)     

       , testProperty "prop_simple" prop_simple
       , testProperty "prop_union" prop_union
       , testProperty "prop_diff" prop_diff
       ]

test_isNF :: Assertion 
test_isNF = fmap not (isNF [(1::Int)..10]) @? "isNF"

checkIsNF :: Map -> Assertion
checkIsNF !m = isNF m @? ("isNF " ++ show m)

-- some simple test data
m0, m1, m2, m3, m5, m6 :: Map
m0 = insert "" (mkA [0,1+2]) empty
m1 = insert "abc" (mkA [1,2,3]) empty
m2 = insert "x" (mkA [0,1]) empty
m3 = insertWith mappend "abc" (mkA [4,5,6]) m1
m5 = singleton "abc" (mkA [42])
m6 = fromList l4

fromList' :: [(d, [Int])] -> [(d, Attr)]
fromList' = fmap (second mkA)

fromList'' :: [(a, Int)] -> [(a, Attr)]
fromList'' = fmap (second $ mkA . return)

fromList''' :: [Key] -> StringMap Attr
fromList''' = fromList . fromList'' . flip zip [1..]

l4 :: [(String, Attr)]
l4 = fromList' [("a",[1]),("b",[2]),("c",[3]),("a",[2]),("ab",[22]),("a",[3])]


prop_simple :: Q.Property
prop_simple = Q.monadicIO $ do
                            l <- Q.pick Q.arbitrary
                            passed <- Q.run $ isNF $! fromList''' l
                            Q.assert passed

prop_union :: Q.Property
prop_union = Q.monadicIO $ do
                            l1 <- Q.pick Q.arbitrary
                            l2 <- Q.pick Q.arbitrary
                            let sm = fromList''' l1 `union` fromList''' l2
                            checkIsNFProp sm
                            

prop_diff :: Q.Property
prop_diff = Q.monadicIO $ do
                            l1 <- Q.pick Q.arbitrary
                            l2 <- Q.pick Q.arbitrary
                            let sm = fromList''' l1 `difference` fromList''' l2
                            checkIsNFProp sm

checkIsNFProp :: a -> Q.PropertyM IO ()                           
checkIsNFProp sm = do
                            passed <- Q.run $ isNF $! sm
                            Q.run $ assertNF $! sm
                            Q.assert passed