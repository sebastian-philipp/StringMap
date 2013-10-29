{-# OPTIONS -XBangPatterns #-}
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
          StringMap
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
        )
import           Data.StringMap.FuzzySearch
import           Prelude                    hiding (lookup, map, mapM, null,
                                             succ)

--import Data.Strict.Tuple
import qualified Data.List      as L
--import Data.BitUtil
--import Data.StrictPair

-- | /O(1)/ Create a map with a single element.

singleton               :: Key -> a -> StringMap a
singleton !k v           = L.foldr (\ c r -> branch c r empty) (val v empty) $ k -- siseq k (val v empty)

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

    ins k  Empty                = singleton k v

    ins k (Val v' t')
        = case k of
          []                    -> flip val t' $! f v v'
          _                     -> val      v'  (ins' k t')

    ins _ _                     = normError "insert'"

-- | /O(n)/ Creates a trie from a list of key\/value pairs.
fromList                        :: [(Key, a)] -> StringMap a
fromList                        = L.foldl' (\p (k, v) -> insert k v p) empty
