module Main
where
import           Data.StringMap


import qualified Data.Char                            as Char (intToDigit)
import qualified Data.List                            as List (nubBy, (!!))
import qualified Data.Map                             as Map (empty, fromList,
                                                              map, toList)
import qualified Data.Set                             as Set (fromList)
import           Prelude                              hiding (filter, foldl,
                                                       foldr, lookup, map, null)

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test, Testable)
import           Text.Show.Functions                  ()


default (Int)

main :: IO ()
main = defaultMain
       [
         -- testCase "exclamation" test_exclamation
         testCase "value" test_value
       , testCase "valueWithDefault" test_valueWithDefault
       , testCase "null" test_null
       , testCase "size" test_size
       , testCase "member" test_member
       , testCase "lookup" test_lookup
       , testCase "findWithDefault" test_findWithDefault
       , testCase "prefixFind" test_prefixFind
       , testCase "prefixFindWithKey" test_prefixFindWithKey
       , testCase "prefixFindWithKeyBF" test_prefixFindWithKeyBF
       , testCase "empty" test_empty
       , testCase "singleton" test_singleton
       , testCase "insert" test_insert
       , testCase "insertWith" test_insertWith
       , testCase "insertWithKey" test_insertWithKey
       , testCase "delete" test_delete
       , testCase "update" test_update
       , testCase "updateWithKey" test_updateWithKey
       , testCase "union" test_union
       , testCase "unionWith" test_unionWith
       , testCase "unionWithKey" test_unionWithKey
       , testCase "difference" test_difference
       , testCase "differenceWith" test_differenceWith
       , testCase "differenceWithKey" test_differenceWithKey
       , testCase "map" test_map
       , testCase "mapWithKey" test_mapWithKey
       , testCase "mapMaybe" test_mapMaybe
       -- , testCase "mapM" test_mapM
       -- , testCase "mapWithKeyM" test_mapWithKeyM
       , testCase "fold" test_fold
       , testCase "foldWithKey" test_foldWithKey
       , testCase "keys" test_keys
       , testCase "elems" test_elems
       , testCase "fromList" test_fromList
       , testCase "toList" test_toList
       , testCase "toListBF" test_toListBF
       , testCase "fromMap" test_fromMap
       , testCase "toMap" test_toMap
       -- , testCase "space" test_space
       -- , testCase "keyChars" test_keyChars
       , testCase "prefixFindCaseWithKey" test_prefixFindCaseWithKey     -- fuzzy search
       , testCase "prefixFindNoCaseWithKey" test_prefixFindNoCaseWithKey
       , testCase "prefixFindNoCase" test_prefixFindNoCase
       , testCase "lookupNoCase" test_lookupNoCase
       , testCase "prefixFindCaseWithKeyBF" test_prefixFindCaseWithKeyBF
       , testCase "prefixFindNoCaseWithKeyBF" test_prefixFindNoCaseWithKeyBF
       -- , testCase "lookupNoCaseBF" test_lookupNoCaseBF
       , testProperty "insert to singleton"  prop_singleton
       , testProperty "map a StringMap" prop_map
       , testProperty "fromList - toList" prop_fromListToList
       , testProperty "space" prop_space
       , testProperty "lookupNoCaseBF is redundant" prop_lookupNoCaseBF
       ]

------------------------------------------------------------------------

type UMap = StringMap ()
type IMap = StringMap Int
type SMap = StringMap String

cmpset :: (Eq a, Show a, Ord a) => [a] -> [a] -> Assertion
cmpset l r = (Set.fromList l) @?= (Set.fromList r)

cmpset' :: (Ord a) => [a] -> [a] -> Bool
cmpset' l r = (Set.fromList l) == (Set.fromList r)

mergeString :: String -> String -> String -> String
mergeString key l r = key ++ ":" ++ l ++ "|" ++ r

----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------

_1, _4 :: Int
_1 = 1
_4 = 4

test_exclamation :: Assertion
test_exclamation = undefined

test_value :: Assertion
test_value =
    let m1 = fromList [("" ,_1),("a", 2)] in
    let m2 = fromList [("x",_1),("a", 2)] in
    do
      value m1 @?= Just _1
      value m2 @?= Nothing

test_valueWithDefault :: Assertion
test_valueWithDefault =
    let m1 = fromList [("" ,_1),("a", 2)] in
    let m2 = fromList [("x",_1),("a", 2)] in
    do
      valueWithDefault 3 m1 @?= _1
      valueWithDefault 3 m2 @?=  3

test_null :: Assertion
test_null =
    let m = fromList [("a",_1), ("ab", 2)] in
    do
      null m @?= False
      null (empty :: UMap) @?= True

test_size :: Assertion
test_size = do
  size (fromList [("a",_1), ("ab", 2)]) @?= 2
  size (fromList [("a",_1), ("a", 2)]) @?= 1
  size (empty :: UMap) @?= 0

test_member :: Assertion
test_member = do
  member "ab" (fromList [("a",_1), ("ab", 2)]) @?= True
  member "aba" (fromList [("a",_1), ("ab", 2)]) @?= False
  member "" (empty :: UMap) @?= False

test_lookup :: Assertion
test_lookup = do
  lookup "ab" (fromList [("a",_1), ("ab", 2)]) @?= Just 2
  lookup "aba" (fromList [("a",_1), ("ab", 2)]) @?= Nothing
  lookup "" (empty :: UMap) @?= Nothing

test_findWithDefault :: Assertion
test_findWithDefault = do
  findWithDefault 7 "ab" (fromList [("a",_1), ("ab", 2)]) @?= 2
  findWithDefault 7 "aba" (fromList [("a",_1), ("ab", 2)]) @?= 7
  findWithDefault 7 "" (empty :: IMap) @?= 7

test_prefixFind :: Assertion
test_prefixFind = do
  prefixFind "a" (fromList [("a",_1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) `cmpset` [1, 2, 4]
  prefixFind "" (fromList [("a",_1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) `cmpset` [1, 2, 3, 4, 5]
  prefixFind "foo" (fromList [("a",_1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) @?= []
  prefixFind "" (empty :: UMap) @?= []

test_prefixFindWithKey :: Assertion
test_prefixFindWithKey = do
  prefixFindWithKey "a" (fromList [("a",_1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) `cmpset`  [("a",_1), ("ab", 2), ("aaa", 4)]
  prefixFindWithKey "" (fromList [("a",_1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) `cmpset`  [("a",_1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]
  prefixFindWithKey "foo" (fromList [("a",_1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) @?= []
  prefixFindWithKey "" (empty :: UMap) @?= []

test_prefixFindWithKeyBF :: Assertion
test_prefixFindWithKeyBF = do
  prefixFindWithKeyBF "a" (fromList [("a",_1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) @?= [("a",_1), ("ab", 2), ("aaa", 4)]
  prefixFindWithKeyBF "" (fromList [("a",_1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) @?=  [("a",_1), ("b", 5), ("ab", 2), ("aaa", 4), ("cab", 3)]
  prefixFindWithKeyBF "foo" (fromList [("a",_1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) @?= []
  prefixFindWithKeyBF "" (empty :: UMap) @?= []

test_empty :: Assertion
test_empty = do
  (empty :: UMap)  @?= fromList []
  size empty @?= 0


test_singleton :: Assertion
test_singleton = do
  singleton "k" 'a'        @?= fromList [("k", 'a')]
  size (singleton "k" 'a') @?= 1

test_insert :: Assertion
test_insert = do
  insert "5" 'x' (fromList [("5",'a'), ("3",'b')]) @?= fromList [("3", 'b'), ("5", 'x')]
  insert "7" 'x' (fromList [("5",'a'), ("3",'b')]) @?= fromList [("3", 'b'), ("5", 'a'), ("7", 'x')]
  insert "5" 'x' empty                         @?= singleton "5" 'x'

test_insertWith :: Assertion
test_insertWith = do
  insertWith (++) "5" "xxx" (fromList [("5","a"), ("3","b")]) @?= fromList [("3", "b"), ("5", "xxxa")]
  insertWith (++) "7" "xxx" (fromList [("5","a"), ("3","b")]) @?= fromList [("3", "b"), ("5", "a"), ("7", "xxx")]
  insertWith (++) "5" "xxx" empty                         @?= singleton "5" "xxx"

test_insertWithKey :: Assertion
test_insertWithKey = do
  insertWithKey mergeString "5" "xxx" (fromList [("5","a"), ("3","b")]) @?= fromList [("3", "b"), ("5", "5:xxx|a")]
  insertWithKey mergeString "7" "xxx" (fromList [("5","a"), ("3","b")]) @?= fromList [("3", "b"), ("5", "a"), ("7", "xxx")]
  insertWithKey mergeString "5" "xxx" empty                         @?= singleton "5" "xxx"

test_delete :: Assertion
test_delete = do
  delete "a" (fromList [("a",_1), ("ab", 2)]) @?= fromList [("ab", 2)]
  delete "ab" (fromList [("a",_1), ("ab", 2)]) @?= fromList [("a", 1)]
  delete "ab" (empty :: IMap) @?= empty

test_update :: Assertion
test_update = do
  update f "a" (fromList [("a",_1), ("ab", 2)]) @?= fromList [("a",777), ("ab", 2)]
  update f "a" (fromList [("a",_4), ("ab", 2)]) @?= fromList [("ab", 2)]
  update f "a" (empty :: IMap) @?= empty
    where
      f 1 = Just 777
      f _ = Nothing

test_updateWithKey :: Assertion
test_updateWithKey = do
  updateWithKey f "a" (fromList [("a","a"), ("ab","b")]) @?= fromList [("ab", "b"), ("a", "a:new a")]
  updateWithKey f "c" (fromList [("","a"), ("ab","b")]) @?= fromList [("ab", "b"), ("", "a")]
  updateWithKey f "ab" (fromList [("","a"), ("ab","b")]) @?= singleton "" "a"
    where
      f k x = if x == "a" then Just ((k) ++ ":new a") else Nothing

test_union :: Assertion
test_union = do
  union (fromList [("a",_1), ("ab", 3)]) (fromList [("a", 2), ("c",_4)]) @?= fromList [("a",_1), ("ab", 3), ("c",_4)]
  union empty (fromList [("a", 2), ("c",_4)]) @?= fromList [("a", 2), ("c",_4)]

test_unionWith :: Assertion
test_unionWith = do
  unionWith (+) (fromList [("a",_1), ("ab", 3)]) (fromList [("a", 2), ("c",_4)]) @?= fromList [("a", 3), ("ab", 3), ("c",_4)]
  unionWith (+) empty (fromList [("a", 2), ("c",_4)]) @?= fromList [("a", 2), ("c",_4)]


test_unionWithKey :: Assertion
test_unionWithKey =
    unionWithKey mergeString (fromList [("a", "a"), ("ab", "b")]) (fromList [("a", "A"), ("c", "C")]) @?= fromList [("ab", "b"), ("a", "a:a|A"), ("c", "C")]

test_difference :: Assertion
test_difference =
    difference (fromList [("a", "a"), ("ab", "b")]) (fromList [("a", "A"), ("c", "C")]) @?= (singleton "ab" "b")

test_differenceWith :: Assertion
test_differenceWith =
    differenceWith f (fromList [("a", "a"), ("ab", "b")]) (fromList [("a", "A"), ("ab", "B"), ("c", "C")]) @?= singleton "ab" "b:B"
    where
      f al ar = if al== "b" then Just (al ++ ":" ++ ar) else Nothing

test_differenceWithKey :: Assertion
test_differenceWithKey =
    differenceWithKey f (fromList [("a", "a"), ("ab", "b")]) (fromList [("a", "A"), ("ab", "B"), ("c", "C")]) @?= singleton "ab" "ab:b|B"
    where
      f k al ar = if al == "b" then Just (mergeString k al ar) else Nothing

test_map :: Assertion
test_map =
    map (* 10) (fromList [("a",_1), ("ab",2)]) @?= fromList [("a", 10), ("ab", 20)]

test_mapWithKey :: Assertion
test_mapWithKey =
    mapWithKey (mergeString "") (fromList [("a","A"), ("ab","B")]) @?= fromList [("ab", ":a|B"), ("a", ":a|A")]

test_mapMaybe :: Assertion
test_mapMaybe = do
    mapMaybe (Just . (* 10)) (fromList [("a",_1), ("ab",2)]) @?= fromList [("a", 10), ("ab", 20)]
    mapMaybe (f) (fromList [("a","A"), ("ab","B")]) @?= fromList [("a", 1::Int)]
    where
        f v = if v == "A" then Just 1 else Nothing

test_mapM :: Assertion
test_mapM = undefined

test_mapWithKeyM :: Assertion
test_mapWithKeyM = undefined

test_fold :: Assertion
test_fold = do
  fold (\ l r -> (Char.intToDigit $ fromIntegral l) : r) "0" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6)]) @?= "45260"
  error "In Which Order?"

test_foldWithKey :: Assertion
test_foldWithKey = do
  foldWithKey f "0" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6)]) @?= "a:4|aa:5|ab:2|b:6|0"
  error "In Which Order?"
    where
      f k l = mergeString k [Char.intToDigit $ fromIntegral l]

test_keys :: Assertion
test_keys =
    keys (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6)]) @?= ["a", "aa", "ab", "b"]

test_elems :: Assertion
test_elems =
    elems (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6)]) @?= [4, 5, 2, 6]

test_fromList :: Assertion
test_fromList = do
  fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6)] @?= fromList [("b", 6), ("ab", 2), ("a",_4), ("aa", 5)]
  fromList [] @?= (empty :: UMap)

test_toList :: Assertion
test_toList = do
  (toList.fromList) [("a",_4), ("ab", 2), ("aa", 5), ("b", 6)] @?= [("a",_4), ("aa", 5), ("ab", 2), ("b", 6)]
  toList (empty :: UMap) @?= []

test_toListBF :: Assertion
test_toListBF = do
  (toListBF.fromList) [("a",_4), ("Ab", 2)] @?= [("a",_4), ("Ab", 2)]
  (toListBF.fromList) [("a",_4), ("ab", 2), ("aa", 5), ("b", 6)] @?= [("a",_4), ("b", 6), ("aa", 5), ("ab", 2)]
  toListBF (empty :: UMap) @?= []


test_fromMap :: Assertion
test_fromMap = do
  fromMap (Map.fromList [("a",_4),("aa",5),("ab",2),("b",6)]) @?= fromList [("a",_4),("aa",5),("ab",2),("b",6)]
  fromMap Map.empty @?= (empty :: UMap)


test_toMap :: Assertion
test_toMap = do
  (toMap.fromList) [("a",_4),("aa",5),("ab",2),("b",6)] @?= Map.fromList [("a",_4),("aa",5),("ab",2),("b",6)]
  toMap (empty :: UMap) @?= Map.empty


test_space :: Assertion
test_space = undefined

test_keyChars :: Assertion
test_keyChars = undefined

test_prefixFindCaseWithKey :: Assertion
test_prefixFindCaseWithKey = do
  prefixFindCaseWithKey "" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("Ab", 7), ("a",_4), ("aa", 5), ("ab", 2), ("b", 6)]
  prefixFindCaseWithKey "a" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("a",_4), ("aa", 5), ("ab", 2)]
  prefixFindCaseWithKey "b" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("b", 6)]
  prefixFindCaseWithKey "c" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= []

test_prefixFindNoCaseWithKey :: Assertion
test_prefixFindNoCaseWithKey = do
  prefixFindNoCaseWithKey "" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("Ab", 7), ("a",_4), ("aa", 5), ("ab", 2), ("b", 6)]
  prefixFindNoCaseWithKey "a" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("Ab", 7), ("a",_4), ("aa", 5), ("ab", 2)]
  prefixFindNoCaseWithKey "b" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("b", 6)]
  prefixFindNoCaseWithKey "c" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= []

test_prefixFindNoCase :: Assertion
test_prefixFindNoCase = do
  prefixFindNoCase "" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("aB", 7)]) @?= [4, 7, 5, 2, 6]
  prefixFindNoCase "a" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("aB", 7)]) @?= [4, 7, 5, 2]
  prefixFindNoCase "b" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("aB", 7)]) @?= [6]
  prefixFindNoCase "c" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("aB", 7)]) @?= []

test_lookupNoCase :: Assertion
test_lookupNoCase =  do
  lookupNoCase "ab" (fromList [("a",_1), ("Ab", 2)]) @?= [("Ab", 2)]
  lookupNoCase "aB" (fromList [("a",_1), ("Ab", 2)]) @?= [("Ab", 2)]
  lookupNoCase "" (empty :: UMap) @?= []

test_prefixFindCaseWithKeyBF :: Assertion
test_prefixFindCaseWithKeyBF = do
  prefixFindCaseWithKeyBF "" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("a",_4), ("b", 6), ("Ab", 7), ("aa", 5), ("ab", 2)]
  prefixFindCaseWithKeyBF "a" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("a",_4), ("aa", 5), ("ab", 2)]
  prefixFindCaseWithKeyBF "b" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("b", 6)]
  prefixFindCaseWithKeyBF "c" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= []


test_prefixFindNoCaseWithKeyBF :: Assertion
test_prefixFindNoCaseWithKeyBF = do
  prefixFindNoCaseWithKeyBF "" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("a",_4), ("b", 6), ("Ab", 7), ("aa", 5), ("ab", 2)]
  prefixFindNoCaseWithKeyBF "a" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("a",_4), ("Ab", 7), ("aa", 5), ("ab", 2)]
  prefixFindNoCaseWithKeyBF "b" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= [("b", 6)]
  prefixFindNoCaseWithKeyBF "c" (fromList [("a",_4), ("ab", 2), ("aa", 5), ("b", 6), ("Ab", 7)]) @?= []

test_lookupNoCaseBF :: Assertion
test_lookupNoCaseBF = undefined

----------------------------------------------------------------
-- QuickCheck
----------------------------------------------------------------

makeUnique :: [(Key, Int)] -> [(Key, Int)]
makeUnique = List.nubBy (\ (f,_) (s,_) -> f == s)

prop_singleton :: Key -> Key -> Bool
prop_singleton k x = insert k x empty == singleton k x

prop_map ::  (Int -> Int) -> [(Key, Int)] -> Bool
prop_map f l = (toListBF.(map f).fromList) l `cmpset'` ((Map.toList).(Map.map f).(Map.fromList)) l

prop_fromListToList :: [(Key, Int)] -> Bool
prop_fromListToList l = ((toList.fromList.makeUnique) l) `cmpset'` (makeUnique l)

prop_space :: [(Key, Int)] -> Bool
prop_space [] = True
prop_space l = (space.fromList) l >= (space.fromList.tail) l

prop_lookupNoCaseBF :: [(Key, Int)] -> Int -> Bool
prop_lookupNoCaseBF [] _ = True
prop_lookupNoCaseBF l  k = (test' lookupNoCaseBF) == (test' lookupNoCase)
    where
      ul = makeUnique l
      key :: Key
      key = fst $ ul List.!! (k `mod` length ul)
      test' :: (Key -> StringMap Int -> [(Key, Int)]) -> [(Key, Int)]
      test' f =  f key (fromList ul)
