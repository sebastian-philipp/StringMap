{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SimpleStrictTest () where

import           Control.Arrow         (second)
import           Control.DeepSeq       (($!!))
import           Data.Monoid
import           Data.StringMap.Strict
import           GHC.AssertNF

import Test.QuickCheck (arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

newtype Attr = A [Int]
    deriving (Show, Monoid)


type Map = StringMap Attr

-- strict constructor for Attr

mkA :: [Int] -> Attr
mkA xs = A $!! xs


testL :: [String] -> IO ()
testL l = check (show l)$ fromList $ fromList' $ zip (l) (fmap (\x -> [x]) [1..])


-- some simple test data
m1, m2, m3 :: Map
m0 = insert "" (mkA [0,1+2]) empty
m1 = insert "abc" (mkA [1,2,3]) empty
m2 = insert "xyz" (mkA [0,1]) empty
m3 = insertWith mappend "abc" (mkA [4,5,6]) m1

fromList' = fmap (second mkA)

fromList'' :: [(a, Int)] -> [(a, Attr)]
fromList'' = fmap (second $ mkA . return)

fromList''' = fromList . fromList'' . flip zip [1..]

l4 = fromList' [("a",[1]),("b",[2]),("c",[3]),("a",[2]),("ab",[22]),("a",[3])]

check :: String -> Map -> IO ()
check msg !m = assertNFNamed msg m

main =
    do check "m0" m0
       check "m1" m1
       check "m2" m2
       check "m3" m3
       check "fromList l4" (fromList l4)
       check "m2 union m3" (m2 `union` m3)
       check "m2 unionWith m2" (unionWith mappend m2 m2)

prop_simple :: Property
prop_simple = monadicIO $ do
                            l <- pick arbitrary
                            passed <- run $ isNF $! fromList''' l
                            assert passed

prop_union :: Property
prop_union = monadicIO $ do
                            l1 <- pick arbitrary
                            l2 <- pick arbitrary
                            let sm = (fromList''' l1 `union` fromList''' l2)
                            passed <- run $ isNF $! sm
                            run $ assertNF $! sm
                            assert passed
