import Data.StringMap

--import Data.Monoid
--import Data.Maybe hiding (mapMaybe)
--import qualified Data.Maybe as Maybe (mapMaybe)
--import Data.Ord
--import Data.Function
import Prelude hiding (lookup, null, map, filter, foldr, foldl)
import qualified Prelude (map)
import qualified Data.Map as Data.Map

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
		 -- , testCase "findWithDefault" test_findWithDefault
		 -- , testCase "prefixFind" test_prefixFind
		 -- , testCase "prefixFindWithKey" test_prefixFindWithKey
		 -- , testCase "prefixFindWithKeyBF" test_prefixFindWithKeyBF
		 , testCase "empty" test_empty
		 , testCase "singleton" test_singleton
		 -- , testCase "insert" test_insert
		 -- , testCase "insertWith" test_insertWith
		 -- , testCase "insertWithKey" test_insertWithKey
		 -- , testCase "delete" test_delete
		 -- , testCase "update" test_update
		 -- , testCase "updateWithKey" test_updateWithKey
		 -- , testCase "union" test_union
		 -- , testCase "unionWith" test_unionWith
		 -- , testCase "unionWithKey" test_unionWithKey
		 -- , testCase "difference" test_difference
		 -- , testCase "differenceWith" test_differenceWith
		 -- , testCase "differenceWithKey" test_differenceWithKey
		 -- , testCase "map" test_map
		 -- , testCase "mapWithKey" test_mapWithKey
		 -- , testCase "mapM" test_mapM
		 -- , testCase "mapWithKeyM" test_mapWithKeyM
		 -- , testCase "fold" test_fold
		 -- , testCase "foldWithKey" test_foldWithKey
		 -- , testCase "keys" test_keys
		 -- , testCase "elems" test_elems
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

----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------


test_exclamation :: Assertion
test_exclamation = undefined

test_value :: Assertion
test_value = let m = fromList [("a",1), ("b", 2)] in do
				value m @?= Nothing
				fail "TODO. How does 'value' this work?"


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
test_findWithDefault = undefined

test_prefixFind :: Assertion
test_prefixFind = undefined

test_prefixFindWithKey :: Assertion
test_prefixFindWithKey = undefined

test_prefixFindWithKeyBF :: Assertion
test_prefixFindWithKeyBF = undefined

test_empty :: Assertion
test_empty = do
	(empty :: UMap)  @?= fromList []
	size empty @?= 0


test_singleton :: Assertion
test_singleton = do
	singleton "k" 'a'        @?= fromList [("k", 'a')]
	size (singleton "k" 'a') @?= 1

test_insert :: Assertion
test_insert = undefined

test_insertWith :: Assertion
test_insertWith = undefined

test_insertWithKey :: Assertion
test_insertWithKey = undefined

test_delete :: Assertion
test_delete = undefined

test_update :: Assertion
test_update = undefined

test_updateWithKey :: Assertion
test_updateWithKey = undefined

test_union :: Assertion
test_union = undefined

test_unionWith :: Assertion
test_unionWith = undefined

test_unionWithKey :: Assertion
test_unionWithKey = undefined

test_difference :: Assertion
test_difference = undefined

test_differenceWith :: Assertion
test_differenceWith = undefined

test_differenceWithKey :: Assertion
test_differenceWithKey = undefined

test_map :: Assertion
test_map = undefined

test_mapWithKey :: Assertion
test_mapWithKey = undefined

test_mapM :: Assertion
test_mapM = undefined

test_mapWithKeyM :: Assertion
test_mapWithKeyM = undefined

test_fold :: Assertion
test_fold = undefined

test_foldWithKey :: Assertion
test_foldWithKey = undefined

test_keys :: Assertion
test_keys = undefined

test_elems :: Assertion
test_elems = undefined

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
prop_map f l = ((toList.(map f).fromList) l) == (((Data.Map.toList).(Data.Map.map f).(Data.Map.fromList)) l) -- if this fails, you may have to sort the list

