-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.Dim2Search
  Copyright  : Copyright (C) 2014 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: portable

  2-dimensional range search of numeric values, e.g. pairs of Ints or Doubles
  using StringMap and prefix search

  Assumption: The coordinates, e.g. Int values are converted into strings
  of equal length such that the ordering is preserved by the lexikographic ordering.

  Example: convert an Int (>= 0) into a String
  @intToString = reverse . take 19 . (++ repeat '0') . reverse . show@

  Do this for both coordinates of a tuple
  @(x,y)::(Int,Int)@
  and merge the two strings character by character.
  The resulting string is used as key and stored together with an attribute
  in a StringMap.

  A range search for all keys within a rectangle @(p1, p2) = ((x1,y1),(x2,y2))@
  in a map @m@ can be done by @lookupGE p1' . lookupLE p2' $ m@ with
  @p1'@ and @p2'@ as the to string converted points of the rectangle.

  @lookupGE p1'@ throws away all keys not located in the quadrant with @p1@
  as lower left corner, @lookupLE p2'@ all key not located in the quadrant
  with @p2@ as upper right corner. So the combination (@lookupRange@) computed
  the intersection of these two quadrants.

  Efficiency of these two function is about the same as a normal lookup
  from StringMap.Base.

  This module should be imported @qualified@, the names in Data.StringMap.Dim2Search are the
  same as theirs siblings in Data.StringMap:

  > import           Data.StringMap (StringMap)
  > import qualified Data.StringMap             as M
  > import qualified Data.StringMap.Dim2Search  as Dim2

-}

-- ----------------------------------------------------------------------------

module Data.StringMap.Dim2Search
-- {-
    ( lookupGE
    , lookupLE
    , lookupRange
    )
-- -}
where

import           Data.StringMap.Base hiding (lookupGE, lookupLE, lookupRange)

-- ----------------------------------------

-- | remove all entries from the map with key less than the argument key

lookupGE                        :: Key -> StringMap a -> StringMap a
lookupGE                        = lookupGE'

lookupGE'                       :: Key -> StringMap a -> StringMap a
lookupGE' k0                    = look k0 . norm
    where

    -- take all values in tree t, they are larger than the key
    look [] t                   = t

    look k@(c : k1) (Branch c' s' n')
        -- this dimension fits for s', the other dimension has to be checked
        -- with lookupGE2, process has to be repeated for the rest
        | c <  c'               = branch c' (lookupGE2 k1 s') rest

        -- symbols are equal, no info about ordering gathered, repeat the
        -- the same lookup for the subtree s'
        -- the rest in n' has to be processed the same way as this branch
        | c == c'               = branch c' (lookupGE' k1 s') rest

        -- this dimension does not fit, throw away this branch and continue with n'
        | otherwise             =                             rest
        where
          rest                  = lookupGE' k n'

    -- empty remains empty
    look _          Empty       = empty

    -- throw away the value, its smaller than required
    look k         (Val _v' t') = lookupGE' k t'

    -- the impossible has happened
    look _ _                    = normError "lookupGE'"

lookupGE2                      :: Key -> StringMap a -> StringMap a
lookupGE2 k0                   = look k0 . norm
    where
    -- key is empty, all values in t are larger, so they are included
    look [] t                   = t

    look k@(c : k1) t@(Branch c' s' n')
        -- tree s' and all others in n' contain values larger than required
        -- take them
        | c <  c'               = t

        -- the 1. symbols are equal, so lookup has to continue,
        -- but only along this dimension, so skip the next key symbol (lookupLE1) and
        -- repeat this comparison procedure (call of lookupLE2 in lookupLE1)
        -- the rest (n') is taken like in the 1. case
        | c == c'               = branch c' (lookupGE1 k1 s') n'

        -- the 1. symbol in the key is larger, so cut off this subtree (s')
        -- and repeat lookup for the rest (n')
        | otherwise             = lookupGE2 k n'

    -- empty remains empty
    look _          Empty       = empty

    -- throw away the value, its smaller than required
    look k         (Val _v' t') = lookupGE2 k t'

    -- the impossible has happened
    look _ _                    = normError "lookupGE2"

lookupGE1                       :: Key -> StringMap a -> StringMap a
lookupGE1 k0               = look k0 . norm
    where
    -- like above
    look [] t                   = t

    -- ignore the 1. symbol of the key, take the subtree s' and
    -- continue comparison of every other symbol,
    -- do the same for all remaining trees in n'
    look k@(_c : k1) (Branch c' s' n')
                                = branch c' (lookupGE2 k1 s') $ lookupGE1 k n'

    -- like above
    look _          Empty       = empty

    -- like above
    look k         (Val _v' t') = lookupGE1 k t'

    -- like above
    look _ _                    = normError "lookupGE1"

-- ----------------------------------------
--
-- the same stuff for less or equal

lookupLE                        :: Key -> StringMap a -> StringMap a
lookupLE                        = lookupLE'

lookupLE'                       :: Key -> StringMap a -> StringMap a
lookupLE' k0                    = look k0 . norm
    where

    -- if key is empty and node stores a value
    -- take this value, it's the upper limit,
    -- all other values in the subtree _t' are larger and thrown away
    look [] (Val v' _t')        = (Val v' empty)

    -- key is empty, all remaining values in _t are larger and thrown away
    look [] _t                  = empty

    look k@(c : k1) (Branch c' s' n')
        -- the char c' is larger than the 1. char in the search key
        -- so this and all other others (n') are cut off
        | c <  c'               =                             empty

        -- the char c and c' are the same, so search for this subtree s' must
        -- continue, but all further trees (n') are cut off
        | c == c'               = branch c' (lookupLE' k1 s') empty

        -- the char c' is smaller than the 1. char in the search key
        -- so concerning this dimension, the elements must be included into the
        -- result, but the other dimension must be checked (with lookupLE2)
        -- all remaining values in n' have also to be taken, therfore the rec. call with n'
        | otherwise             = branch c' (lookupLE2 k1 s') (lookupLE' k n')

    -- the empty tree remains empty
    look _          Empty       = empty

    -- the values v' are included into the result, and the lookup process
    -- continues with the subtree t'
    -- this case will not occur, when the 2-dim keys are normalized and all
    -- are of the same length, in that case the values occur only on leaf nodes not in inner nodes
    look k         (Val v' t')  = val v' (lookupLE' k t')

    -- the impossible has happend
    look _ _                    = normError "lookupLE'"

lookupLE2                      :: Key -> StringMap a -> StringMap a
lookupLE2 k0                   = look k0 . norm
    where

    -- if key is empty and node stores a value
    -- take this value, it's the upper limit,
    -- all other values in the subtree _t' are larger and thrown away
    look [] (Val v' _t')        = (Val v' empty)

    -- key is empty, all remaining values in _t are larger and thrown away
    look [] _t                  = empty

    look k@(c : k1) (Branch c' s' n')
        -- tree s' and all others in n' contain values larger than required
        -- throw them away
        | c <  c'               =                             empty

        -- the 1. symbols are equal, so lookup has to continue,
        -- but only along this dimension, so skip the next key symbol (lookupLE1) and
        -- repeat this comparison procedure (call of lookupLE2 in lookupLE1)
        -- the rest (n') can be thrown away like in the 1. case
        | c == c'               = branch c' (lookupLE1 k1 s') empty

        -- the 1. symbol in the key is larger, so take this subtree (s')
        -- and repeat lookup for the rest (n')
        | otherwise             = branch c' s'                (lookupLE2 k n')

    -- the empty tree remains empty
    look _          Empty       = empty

    -- the values v' are included into the result, and the lookup process
    -- continues with the subtree t'
    -- this case will not occur, when the 2-dim keys are normalized and all
    -- are of the same length, in that case the values occur only on leaf nodes not in inner nodes
    look k         (Val v' t')  = val v' (lookupLE2 k t')

    -- the impossible has happend
    look _ _                    = normError "lookupLE2"

lookupLE1                       :: Key -> StringMap a -> StringMap a
lookupLE1 k0                    = look k0 . norm
    where
    -- like above
    look [] (Val v' _t')        = (Val v' empty)

    -- like above
    look [] t                   = t

    -- ignore the 1. symbol of the key, take the subtree s' and
    -- continue comparison of every other symbol,
    -- do the same for all remaining trees in n'
    look k@(_c : k1) (Branch c' s' n')
                                = branch c' (lookupLE2 k1 s') (lookupLE1 k n')

    -- like above
    look _          Empty       = empty

    -- like above
    look k         (Val v' t')  = val v' (lookupLE1 k t')

    -- like above
    look _ _                    = normError "lookupLE1"


-- | Combination of 'lookupLE' and 'lookupGE'
--
-- > keys $ lookupRange "a" "b" $ fromList $ zip ["", "a", "ab", "b", "ba", "c"] [1..] = ["a","ab","b"]
--
-- For all keys in @k = keys $ lookupRange lb ub m@, this property holts true: @k >= ub && k <= lb@

lookupRange                     :: Key -> Key -> StringMap a -> StringMap a
lookupRange lb ub               = lookupGE lb . lookupLE ub

-- ----------------------------------------

normError               :: String -> a
normError               = normError' "Data.StringMap.Dim2Search"

-- ----------------------------------------

