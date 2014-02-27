{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Main where

import           Data.List                 (sort)
import qualified Data.StringMap            as M
import qualified Data.StringMap.Dim2Search as D2

-- ----------------------------------------
--
-- auxiliary functions for mapping pairs of Ints to Strings and vice versa

intToKey :: Int -> Int -> Int -> String
intToKey base len val = tok len val ""
    where
      tok 0 _ acc = acc
      tok i v acc = tok (i - 1) v' (d : acc)
          where
            (v', r) = v `divMod` base
            d       = toEnum (r + fromEnum '0')

intPairToKey :: Int -> Int -> (Int, Int) -> String
intPairToKey base len (x, y) = merge x' y'
    where
      x' = intToKey base len x
      y' = intToKey base len y

merge :: [a] -> [a] -> [a]
merge []       []       = []
merge (x : xs) (y : ys) = x : y : merge xs ys

intFromKey :: String -> Int
intFromKey = read

unMerge :: [a] -> ([a], [a])
unMerge [] = ([], [])
unMerge (x : y : s) = (x : xs, y : ys)
    where
      (xs, ys) = unMerge s

-- ----------------------------------------
--
-- experiment to understand 2-dimensional location
-- search implemented by using the StringMap impl.
--
-- an ordering on strings (representing pairs of ints)
-- that is isomorphic to the partial ordering
-- used for 2-dimensional search

instance Ord Point' where
    (P' s1) <= (P' s2) = s1 `le` s2
        where
          le [] [] = True
          le (x1 : y1 : ds1) (x2 : y2 : ds2)
              | x1 == x2 && y1 == y2 = ds1 `le`  ds2
              | x1 == x2 && y1 <  y2 = ds1 `leX` ds2
              | x1 <  x2 && y1 == y2 = ds1 `leY` ds2
              | x1 <  x2 && y1 <  y2 = True
              | otherwise            = False

          leX [] [] = True                      -- the result for the Y dimension is already known
          leX (x1 : y1 : ds1) (x2 : y2 : ds2)
              | x1 == x2  = ds1 `leX` ds2
              | x1 <  x2  = True
              | otherwise = False

          leY [] [] = True                      -- the result for the X dimension is already known
          leY (x1 : y1 : ds1) (x2 : y2 : ds2)
              | y1 == y2  = ds1 `leY` ds2
              | y1 <  y2  = True
              | otherwise = False

-- toPoint' and fromPoint': the bijection Point <-> Point'

toPoint' :: Point -> Point'
toPoint' (P p) = P' $ intPairToKey base len p
    where
      base =  2         -- or 10
      len  =  10        -- or  3  (or something else)

fromPoint' :: Point' -> Point
fromPoint' (P' ds) = P (intFromKey xs, intFromKey ys)
    where
      (xs, ys) = unMerge ds

-- the test, whether the `le` ordering is preserved, when working with Point'
propOrdered :: Point -> Point -> Bool
propOrdered p1 p2
    = (p1 `le` p2) == (toPoint' p1 <= toPoint' p2)

-- very quick check test
propTest :: Int -> [(Point, Point)]
propTest n
    = filter (not . uncurry propOrdered) qs
      where
        xs = [1..n]
        ps = [P (x, y) | x <- xs, y <- xs]
        qs = [(p1, p2) | p1 <- ps, p2 <- ps]

test1 :: Bool
test1 = null $ propTest 20

-- ----------------------------------------

newtype Point     = P   {unP :: (Int, Int)    } deriving (Eq)
newtype PointSet  = PS  {unPS :: [Point]       } deriving (Eq)
                                                -- assuming only smart constructor mkPS is used

newtype Point'    = P'  {unP' :: String        } deriving (Eq)
newtype PointSet' = PS' {unPS' :: M.StringMap ()} deriving (Eq)

instance Show Point     where show = show . unP
instance Show Point'    where show = show . unP'
instance Show PointSet  where show = show . unPS
instance Show PointSet' where show = show . M.keys . unPS'

class PartOrd a where
    le :: a -> a -> Bool
    ge :: a -> a -> Bool

instance PartOrd Point where
    (P (x1, y1)) `le` (P (x2, y2))
        = x1 <= x2 && y1 <= y2

    (P (x1, y1)) `ge` (P (x2, y2))
        = x1 >= x2 && y1 >= y2

instance PartOrd Point' where
    (P' p1) `le` (P' p2)
        = not . M.null . D2.lookupLE p2 $ (M.singleton p1 ())

    (P' p1) `ge` (P' p2)
        = not . M.null . D2.lookupGE p2 $ (M.singleton p1 ())

class Lookup p s | s -> p where
    lookupLE :: p -> s -> s
    lookupGE :: p -> s -> s

instance Lookup Point PointSet where
    lookupLE p ps = PS . filter (`le` p) . unPS $ ps
    lookupGE p ps = PS . filter (`ge` p) . unPS $ ps

instance Lookup Point' PointSet' where
    lookupLE p ps = PS' . D2.lookupLE (unP' p) . unPS' $ ps
    lookupGE p ps = PS' . D2.lookupGE (unP' p) . unPS' $ ps

-- the bijection between Point and Point'

pToP' :: Point -> Point'
pToP' = P' . intPairToKey 10 5 . unP    -- base 10, 5 digits

p'ToP :: Point' -> Point
p'ToP (P' p') = P (intFromKey xs, intFromKey ys)
    where
      (xs, ys) = unMerge p'

-- the bijection between PointSet and PointSet'

psToPS' :: PointSet -> PointSet'
psToPS' = PS' . M.fromList . map (\(P' x) -> (x, ())) . map pToP' . unPS

ps'ToPS :: PointSet' -> PointSet
ps'ToPS = mkPS . map (unP . p'ToP . P') . M.keys . unPS'

mkP :: Int -> Int -> Point
mkP x y = P (x, y)

mkP' :: Int -> Int -> Point'
mkP' x y = pToP' $ mkP x y

mkPS :: [(Int, Int)] -> PointSet
mkPS = PS . map P . sort

mkPS' :: [(Int, Int)] -> PointSet'
mkPS' = psToPS' . mkPS

mkxx :: Int -> Point
mkxx i = mkP i i

mkxx' :: Int -> Point'
mkxx' = pToP' . mkxx

mkD2 :: [Int] -> PointSet
mkD2 = PS . map mkxx

mkD2' :: [Int] -> PointSet'
mkD2' = psToPS' . mkD2

d1 :: PointSet
d1 = mkD2 [1,10,100,105,107,125,200, 205, 222]

d1' :: PointSet'
d1' = psToPS' d1

d2 :: PointSet
d2 = mkD2 [2,10,20,25,100,111,155,200,333,500]

d2' :: PointSet'
d2' = psToPS' d2

d0' :: PointSet'
d0' = mkD2' [10,100]


mkSquare :: Int -> Int -> PointSet
mkSquare n m = mkPS [(i, j) | i <- [n..m], j <- [n..m]]

-- input list must contain at least 3 different elements
mkPointPointSet :: [Int] -> ([Point], PointSet)
mkPointPointSet xs0
    = (ps, ps')
      where
        xs@(_ : ys@(_:_:_)) = sort xs0
        xs'                  = init ys
        ps  =      [mkP i j | i <- xs,  j <- xs ]
        ps' = mkPS [ (i, j) | i <- xs', j <- xs']


ps1 :: PointSet
xs1 :: [Point]
(xs1, ps1) =  mkPointPointSet [1,2,10,20,25,100,111,155,200,333,500,505]

lawBijection :: PointSet -> Bool
lawBijection ps
    = ps == (ps'ToPS . psToPS' $ ps)

lawPredicateMorphism :: (Point -> Bool) -> (Point' -> Bool) ->
                        Point -> Bool
lawPredicateMorphism p p' x
    = p x == (p' $ pToP' x)

lawPredicate2Morphism :: (Point -> Point -> Bool) -> (Point' -> Point' -> Bool) ->
                         Point -> Point -> Bool
lawPredicate2Morphism p2 p2' x y
    = lawPredicateMorphism (p2 x) (p2' $ pToP' x) y

lawPointSetMorphism :: (PointSet -> PointSet) -> (PointSet' -> PointSet') ->
                       PointSet -> Bool
lawPointSetMorphism f f' ps
    = f ps == (ps'ToPS . f' . psToPS' $ ps)

lawLookupGE :: Point -> PointSet -> Bool
lawLookupGE p ps = lawPointSetMorphism (lookupGE p) (lookupGE $ pToP' p) ps

lawLookupLE :: Point -> PointSet -> Bool
lawLookupLE p ps = lawPointSetMorphism (lookupLE p) (lookupLE $ pToP' p) ps

testPointPointSet :: (Point -> PointSet -> Bool) -> ([Point], PointSet) -> [Point]
testPointPointSet law (xs, ps)
    = filter (\p -> not $ law p ps) xs

testLookup :: ([Point], PointSet) -> Bool
testLookup ps
    = null (testPointPointSet lawLookupLE ps)
      &&
      null (testPointPointSet lawLookupGE ps)

theTest :: Bool
theTest = testLookup $
       mkPointPointSet [1,2,10,20,25,100,111,155,200,333,500,505]

main :: IO ()
main = print theTest >> return ()

-- ----------------------------------------

