import Data.StringMap

--import Data.Monoid
--import Data.Maybe hiding (mapMaybe)
--import qualified Data.Maybe as Maybe (mapMaybe)
--import Data.Ord
--import Data.Function
import Prelude hiding (lookup, null, map, filter, foldr, foldl)
import qualified Prelude (map)

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
