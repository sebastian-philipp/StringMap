{-# LANGUAGE BangPatterns #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.Strict
  Copyright  : Copyright (C) 2009-2012 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: not portable

  An efficient implementation of maps from strings to arbitrary values.

  Values can associated with an arbitrary byte key. Searching for keys is very fast, but
  the prefix tree probably consumes more memory than "Data.Map". The main differences are the special
  'prefixFind' functions, which can be used to perform prefix queries. The interface is
  heavily borrowed from "Data.Map" and "Data.IntMap".

  Most other function names clash with "Prelude" names, therefore this module is usually
  imported @qualified@, e.g.

  > import Data.StringMap (StringMap)
  > import qualified Data.StringMap as T

  Many functions have a worst-case complexity of /O(min(n,L))/. This means that the operation
  can become linear with the number of elements with a maximum of /L/, the length of the
  key (the number of bytes in the list). The functions for searching a prefix have a worst-case
  complexity of /O(max(L,R))/. This means that the operation can become linear with
  /R/, the number of elements found for the prefix, with a minimum of /L/.

  The module exports include the internal data types, their constructors and access
  functions for ultimate flexibility. Derived modules should not export these
  (as shown in "Holumbus.Data.StrMap") to provide only a restricted interface.

-}

-- ----------------------------------------------------------------------------

module Data.StringMap.Strict
        (
        -- * Map type
          StringMap()
        , Key

        -- * Operators
        , (!)

        -- * Query
        , value
        , valueWithDefault
        , null
        , size
        , member
        , lookup
        , findWithDefault
        , prefixFind
        , prefixFindWithKey
        , prefixFindWithKeyBF
        , lookupRange
        , between

        -- * Construction
        , empty
        , singleton

        -- ** Insertion
        , insert
        , insertWith
        , insertWithKey

        -- ** Delete\/Update
        , delete
        , update
        , updateWithKey

        -- * Combine
        -- ** Union
        , union
        , unionWith
        , unionWithKey

        -- ** Difference
        , difference
        , differenceWith
        , differenceWithKey


        -- * Traversal
        -- ** Map
        , map
        , mapWithKey
        , mapM
        , mapWithKeyM
        , mapMaybe

        -- * Folds
        , fold
        , foldWithKey

        -- * Conversion
        , keys
        , elems

        -- ** Lists
        , fromList
        , toList
        , toListBF

        -- ** Maps
        , fromMap
        , toMap

        -- * Debugging
        , space
        , keyChars

        -- * Prefix and Fuzzy Search
        , prefixFindCaseWithKey     -- fuzzy search
        , prefixFindNoCaseWithKey
        , prefixFindNoCase
        , lookupNoCase

        , prefixFindCaseWithKeyBF
        , prefixFindNoCaseWithKeyBF
        , lookupNoCaseBF
        )
where

import           Data.StringMap.Base hiding
        (
          singleton
        , insert
        , insertWith
        , insertWithKey
        , fromList
        , union
        , unionWith
        )
import           Data.StringMap.FuzzySearch
import           Prelude                    hiding (lookup, map, mapM, null,
                                             succ)

--import Data.Strict.Tuple
import qualified Data.List                  as L
--import Data.BitUtil
--import Data.StrictPair

-- | /O(1)/ Create a map with a single element.
--
-- the attribute value is evaluated to WHNF

singleton               :: Key -> a -> StringMap a
singleton k !v          = siseq (fromKey k) (val v empty)

{- OLD
singleton               :: Key -> a -> StringMap a
singleton !k v           = L.foldr (\ c r -> branch c r empty) (val v empty) $ k -- siseq k (val v empty)
-}

{-# INLINE singleton #-}

-- | /O(min(n,L))/ Insert a new key and value into the map. If the key is already present in
-- the map, the associated value will be replaced with the new value.

insert                          :: Key -> a -> StringMap a -> StringMap a
insert !k !v                    = insertWith const k v

{-# INLINE insert #-}

-- | /O(min(n,L))/ Insert with a combining function. If the key is already present in the map,
-- the value of @f new_value old_value@ will be inserted.

insertWith                      :: (a -> a -> a) -> Key -> a -> StringMap a -> StringMap a
insertWith f !k v t              = insert' f v k t

{-# INLINE insertWith #-}

-- | /O(min(n,L))/ Insert with a combining function. If the key is already present in the map,
-- the value of @f key new_value old_value@ will be inserted.

insertWithKey                   :: (Key -> a -> a -> a) -> Key -> a -> StringMap a -> StringMap a
insertWithKey f !k               = insertWith (f k) k

-- ----------------------------------------

insert'                         :: (a -> a -> a) -> a -> Key -> StringMap a -> StringMap a
insert' f v k0                  = ins k0 . norm
    where
    ins'                        = insert' f v

    ins k (Branch c' s' n')
        = case k of
          []                    -> val v (branch c' s' n')
          (c : k1)
              | c <  c'         -> branch c (singleton k1 v) (branch c' s' n')
              | c == c'         -> branch c (ins' k1 s')                   n'
              | otherwise       -> branch c'         s'            (ins' k n')

    ins k  Empty                = singleton k v                   -- WHNF for v is done in singleton

    ins k (Val v' t')
        = case k of
          []                    -> flip val t' $! f v v'          -- force WHNF of attr value
          _                     -> val      v'  (ins' k t')

    ins _ _                     = normError "insert'"

-- | /O(n)/ Creates a trie from a list of key\/value pairs.
fromList                        :: [(Key, a)] -> StringMap a
fromList                        = L.foldl' (\p (k, v) -> insert k v p) empty

-- ----------------------------------------

-- | /O(n+m)/ Left-biased union of two maps. It prefers the first map when duplicate keys are
-- encountered, i.e. ('union' == 'unionWith' 'const').

union                                           :: StringMap a -> StringMap a -> StringMap a
union                                           = union' const

-- | /O(n+m)/ Union with a combining function.

unionWith                                       :: (a -> a -> a) -> StringMap a -> StringMap a -> StringMap a
unionWith                                       = union'

-- like union' from Base, but attr value is evaluated to WHNF

union'                                          :: (a -> a -> a) -> StringMap a -> StringMap a -> StringMap a
union' f pt1 pt2                                = uni (norm pt1) (norm pt2)
    where
    uni' t1' t2'                                = union' f (norm t1') (norm t2')

    uni     Empty                Empty          = empty
    uni     Empty               (Val v2 t2)     = val v2 t2
    uni     Empty               (Branch c2 s2 n2)
                                                = branch c2 s2 n2

    uni    (Val v1 t1)           Empty          = val    v1     t1
    uni    (Val v1 t1)          (Val v2 t2)     = flip val (uni' t1 t2) $! (f v1 v2)
                                                  -- force attr value evaluation to WHNF
    uni    (Val v1 t1)       t2@(Branch _ _ _)  = val    v1     (uni' t1 t2)

    uni    (Branch c1 s1 n1)     Empty          = branch c1 s1 n1
    uni t1@(Branch _  _  _ )    (Val v2 t2)     = val v2 (uni' t1 t2)
    uni t1@(Branch c1 s1 n1) t2@(Branch c2 s2 n2)
        | c1 <  c2                              = branch c1       s1     (uni' n1 t2)
        | c1 >  c2                              = branch c2          s2  (uni' t1 n2)
        | otherwise                             = branch c1 (uni' s1 s2) (uni' n1 n2)
    uni _                    _                  = normError "union'"

