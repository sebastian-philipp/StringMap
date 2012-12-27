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
         [ testCase "empty" test_empty
         , testCase "singleton" test_singleton
         , testProperty "insert to singleton"  prop_singleton
         , testProperty "map a StringMap" prop_map
         ]

------------------------------------------------------------------------

type UMap = StringMap ()
type IMap = StringMap Int
type SMap = StringMap String

----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------

test_empty :: Assertion
test_empty = do
    (empty :: UMap)  @?= fromList []
    size empty @?= 0

test_singleton :: Assertion
test_singleton = do
    singleton "k" 'a'        @?= fromList [("k", 'a')]
    size (singleton "k" 'a') @?= 1

----------------------------------------------------------------
-- QuickCheck
----------------------------------------------------------------

prop_singleton :: Key -> Key -> Bool
prop_singleton k x = insert k x empty == singleton k x

prop_map ::  (Int -> Int) -> [(Key, Int)] -> Bool
prop_map f l = ((toList.(map f).fromList) l) == (((Data.Map.toList).(Data.Map.map f).(Data.Map.fromList)) l) -- if this fails, you may have to sort the list

