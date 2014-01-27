{-# LANGUAGE BangPatterns #-}

module SimpleStrictTest where

import           Control.Arrow         (second)
import           Control.DeepSeq       (($!!))
import           Data.Monoid
import           Data.StringMap.Strict
import           GHC.AssertNF

newtype Attr = A [Int]
    deriving (Show)

instance Monoid Attr where
    mempty = mkA []
    mappend (A xs) (A ys) = mkA $ xs ++ ys

type Map = StringMap Attr

-- strict constructor for Attr

mkA :: [Int] -> Attr
mkA xs = A $!! xs

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

l4 :: [(String, Attr)]
l4 = fromList' [("a",[1]),("b",[2]),("c",[3]),("a",[2]),("ab",[22]),("a",[3])]

consA :: Int -> Attr -> Attr
consA n a = mkA [n] `mappend` a

check :: String -> Map -> IO ()
check msg !m
    = do putStrLn msg
         assertNFNamed msg m

main :: IO ()
main =
    do check "m0" m0
       check "m5" m5
       check "m1" m1
       check "m2" m2
       check "m3" m3
       check "fromList l4" (fromList l4)
       check "m2 union m3" (m2 `union` m3)
       check "m2 unionWith m2" (unionWith mappend m2 m2)
       check "adjust m6" (adjust (consA 42) "ab" m6)
       check "adjust m1" (adjust (consA 42) "xx" m1)
       check "delete m6" (delete "ab" m6)
       check "delete m1" (delete "xx" m1)


