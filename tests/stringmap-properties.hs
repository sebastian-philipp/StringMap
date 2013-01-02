import Data.StringMap


--import Data.Monoid
--import Data.Maybe hiding (mapMaybe)
--import qualified Data.Maybe as Maybe (mapMaybe)
--import Data.Ord
--import Data.Function
import Prelude hiding (lookup, null, map, filter, foldr, foldl)
import qualified Prelude (map)
import qualified Data.Map as Data.Map
import qualified Data.Set as Set (Set, fromList)
import qualified Data.Char as Char (intToDigit)

--import Data.List (nub,sort)
--import qualified Data.List as List
--import qualified Data.Set
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test, Testable)
import Test.QuickCheck
import Text.Show.Functions ()


default (Int)

main :: IO ()
main = defaultMain
		 [
		 -- testCase "exclamation" test_exclamation
		  testCase "value" test_value
		 -- , testCase "valueWithDefault" test_valueWithDefault
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
		 -- , testCase "mapM" test_mapM
		 -- , testCase "mapWithKeyM" test_mapWithKeyM
		  , testCase "fold" test_fold
		  , testCase "foldWithKey" test_foldWithKey
		  , testCase "keys" test_keys
		  , testCase "elems" test_elems
		 -- , testCase "fromList" test_fromList
		 -- , testCase "toList" test_toList
		 -- , testCase "toListBF" test_toListBF
		 -- , testCase "fromMap" test_fromMap
		 -- , testCase "toMap" test_toMap
		 -- , testCase "space" test_space
		 -- , testCase "keyChars" test_keyChars
		 -- , testCase "prefixFindCaseWithKey" test_prefixFindCaseWithKey     -- fuzzy search
		 -- , testCase "prefixFindNoCaseWithKey" test_prefixFindNoCaseWithKey
		 -- , testCase "prefixFindNoCase" test_prefixFindNoCase
		 -- , testCase "lookupNoCase" test_lookupNoCase
		 -- , testCase "prefixFindCaseWithKeyBF" test_prefixFindCaseWithKeyBF
		 -- , testCase "prefixFindNoCaseWithKeyBF" test_prefixFindNoCaseWithKeyBF
		 -- , testCase "lookupNoCaseBF" test_lookupNoCaseBF         , testProperty "insert to singleton"  prop_singleton
         , testProperty "map a StringMap" prop_map
         ]

------------------------------------------------------------------------

type UMap = StringMap ()
type IMap = StringMap Int
type SMap = StringMap String

cmpset :: (Eq a, Show a, Ord a) => [a] -> [a] -> Assertion
cmpset l r = (Set.fromList l) @?= (Set.fromList r)

mergeString key l r = key ++ ":" ++ l ++ "|" ++ r

----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------


test_exclamation :: Assertion
test_exclamation = undefined

test_value :: Assertion
test_value = let m = fromList [("a",1), ("b", 2)] in do
				value m @?= Nothing
				error "TODO. How does 'value' work?"


test_valueWithDefault :: Assertion
test_valueWithDefault = undefined

test_null :: Assertion
test_null = let m = fromList [("a",1), ("ab", 2)] in do
				null m @?= False
				null (empty :: UMap) @?= True

test_size :: Assertion
test_size = do
				size (fromList [("a",1), ("ab", 2)]) @?= 2
				size (fromList [("a",1), ("a", 2)]) @?= 1
				size (empty :: UMap) @?= 0

test_member :: Assertion
test_member = do
				member "ab" (fromList [("a",1), ("ab", 2)]) @?= True
				member "aba" (fromList [("a",1), ("ab", 2)]) @?= False
				member "" (empty :: UMap) @?= False

test_lookup :: Assertion
test_lookup = do
				lookup "ab" (fromList [("a",1), ("ab", 2)]) @?= Just 2
				lookup "aba" (fromList [("a",1), ("ab", 2)]) @?= Nothing
				lookup "" (empty :: UMap) @?= Nothing

test_findWithDefault :: Assertion
test_findWithDefault = do
				findWithDefault 7 "ab" (fromList [("a",1), ("ab", 2)]) @?= 2
				findWithDefault 7 "aba" (fromList [("a",1), ("ab", 2)]) @?= 7
				findWithDefault 7 "" (empty :: IMap) @?= 7

test_prefixFind :: Assertion
test_prefixFind = do
				prefixFind "a" (fromList [("a",1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) `cmpset` [1, 2, 4]
				prefixFind "" (fromList [("a",1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) `cmpset` [1, 2, 3, 4, 5]
				prefixFind "foo" (fromList [("a",1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) @?= []
				prefixFind "" (empty :: UMap) @?= []

test_prefixFindWithKey :: Assertion
test_prefixFindWithKey = do
				prefixFindWithKey "a" (fromList [("a",1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) `cmpset`  [("a",1), ("ab", 2), ("aaa", 4)]
				prefixFindWithKey "" (fromList [("a",1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) `cmpset`  [("a",1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]
				prefixFindWithKey "foo" (fromList [("a",1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) @?= []
				prefixFindWithKey "" (empty :: UMap) @?= []

test_prefixFindWithKeyBF :: Assertion
test_prefixFindWithKeyBF = do
				prefixFindWithKey "a" (fromList [("a",1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) @?= [("a",1), ("ab", 2), ("aaa", 4)]
				prefixFindWithKey "" (fromList [("a",1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) @?=  [("a",1), ("b", 5), ("ab", 2), ("cab", 3), ("aaa", 4)]
				prefixFindWithKey "foo" (fromList [("a",1), ("ab", 2), ("cab", 3), ("aaa", 4), ("b", 5)]) @?= []
				prefixFindWithKey "" (empty :: UMap) @?= []

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
				delete "a" (fromList [("a",1), ("ab", 2)]) @?= fromList [("ab", 2)]
				delete "ab" (fromList [("a",1), ("ab", 2)]) @?= fromList [("a", 1)]
				delete "ab" (empty :: IMap) @?= empty

test_update :: Assertion
test_update = do
				update f "a" (fromList [("a",1), ("ab", 2)]) @?= fromList [("a",777), ("ab", 2)]
				update f "a" (fromList [("a",4), ("ab", 2)]) @?= fromList [("ab", 2)]
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
	union (fromList [("a", 1), ("ab", 3)]) (fromList [("a", 2), ("c", 4)]) @?= fromList [("a", 1), ("ab", 3), ("c", 4)]
	union empty (fromList [("a", 2), ("c", 4)]) @?= fromList [("a", 2), ("c", 4)]

test_unionWith :: Assertion
test_unionWith = do
	unionWith (+) (fromList [("a", 1), ("ab", 3)]) (fromList [("a", 2), ("c", 4)]) @?= fromList [("a", 3), ("ab", 3), ("c", 4)]
	unionWith (+) empty (fromList [("a", 2), ("c", 4)]) @?= fromList [("a", 2), ("c", 4)]


test_unionWithKey :: Assertion
test_unionWithKey = unionWithKey mergeString (fromList [("a", "a"), ("ab", "b")]) (fromList [("a", "A"), ("c", "C")]) @?= fromList [("ab", "b"), ("a", "a:a|A"), ("c", "C")]

test_difference :: Assertion
test_difference = difference (fromList [("a", "a"), ("ab", "b")]) (fromList [("a", "A"), ("c", "C")]) @?= (singleton "ab" "b")

test_differenceWith :: Assertion
test_differenceWith = differenceWith f (fromList [("a", "a"), ("ab", "b")]) (fromList [("a", "A"), ("ab", "B"), ("c", "C")]) @?= singleton "ab" "b:B"
	where
	f al ar = if al== "b" then Just (al ++ ":" ++ ar) else Nothing

test_differenceWithKey :: Assertion
test_differenceWithKey = differenceWithKey f (fromList [("a", "a"), ("ab", "b")]) (fromList [("a", "A"), ("ab", "B"), ("c", "C")]) @?= singleton "ab" "ab:b|B"
	where
	f k al ar = if al == "b" then Just (mergeString k al ar) else Nothing

test_map :: Assertion
test_map = map (* 10) (fromList [("a",1), ("ab",2)]) @?= fromList [("a", 10), ("ab", 20)]

test_mapWithKey :: Assertion
test_mapWithKey = mapWithKey (mergeString "") (fromList [("a","A"), ("ab","B")]) @?= fromList [("ab", ":a|B"), ("a", ":a|A")]

test_mapM :: Assertion
test_mapM = undefined

test_mapWithKeyM :: Assertion
test_mapWithKeyM = undefined

test_fold :: Assertion
test_fold = do
	fold (\ l r -> [Char.intToDigit $ fromIntegral l] ++ r) "0" (fromList [("a",4), ("ab", 2), ("aa", 5), ("b", 6)]) @?= "45260"
	error "In Which Order?"

test_foldWithKey :: Assertion
test_foldWithKey = do
	foldWithKey f "0" (fromList [("a",4), ("ab", 2), ("aa", 5), ("b", 6)]) @?= "a:4|aa:5|ab:2|b:6|0"
	error "In Which Order?"
	where
	f k l r = mergeString k [Char.intToDigit $ fromIntegral l] r

test_keys :: Assertion
test_keys = keys (fromList [("a",4), ("ab", 2), ("aa", 5), ("b", 6)]) @?= ["a", "aa", "ab", "b"]

test_elems :: Assertion
test_elems = elems (fromList [("a",4), ("ab", 2), ("aa", 5), ("b", 6)]) @?= [4, 5, 2, 6]

test_fromList :: Assertion
test_fromList = undefined

test_toList :: Assertion
test_toList = undefined

test_toListBF :: Assertion
test_toListBF = undefined

test_fromMap :: Assertion
test_fromMap = undefined

test_toMap :: Assertion
test_toMap = undefined

test_space :: Assertion
test_space = undefined

test_keyChars :: Assertion
test_keyChars = undefined

test_prefixFindCaseWithKey :: Assertion
test_prefixFindCaseWithKey = undefined

test_prefixFindNoCaseWithKey :: Assertion
test_prefixFindNoCaseWithKey = undefined

test_prefixFindNoCase :: Assertion
test_prefixFindNoCase = undefined

test_lookupNoCase :: Assertion
test_lookupNoCase = undefined

test_prefixFindCaseWithKeyBF :: Assertion
test_prefixFindCaseWithKeyBF = undefined

test_prefixFindNoCaseWithKeyBF :: Assertion
test_prefixFindNoCaseWithKeyBF = undefined

test_lookupNoCaseBF :: Assertion
test_lookupNoCaseBF = undefined

----------------------------------------------------------------
-- QuickCheck
----------------------------------------------------------------

prop_singleton :: Key -> Key -> Bool
prop_singleton k x = insert k x empty == singleton k x

prop_map ::  (Int -> Int) -> [(Key, Int)] -> Bool
prop_map f l = ((Set.fromList).toListBF.(map f).fromList) l == ((Set.fromList).(Data.Map.toList).(Data.Map.map f).(Data.Map.fromList)) l

