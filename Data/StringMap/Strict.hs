{-# LANGUAGE BangPatterns #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.Strict
  Copyright  : Copyright (C) 2009-2013 Uwe Schmidt, Sebastian Philipp
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: not portable

  An efficient implementation of maps from strings to arbitrary values.

  Values can be associated with an arbitrary [Char] key. Searching for keys is very fast.
  The main differences to Data.Map and Data.IntMap are the special
  'prefixFind' functions, which can be used to perform prefix queries. The interface is
  heavily borrowed from "Data.Map" and "Data.IntMap".

  Most other function names clash with "Prelude" names, therefore this module is usually
  imported @qualified@, e.g.

  > import           Data.StringMap.Strict (StringMap)
  > import qualified Data.StringMap.Strict as M

  Many functions have a worst-case complexity of /O(min(n,L))/. This means that the operation
  can become linear with the number of elements with a maximum of /L/, the length of the
  key (the number of bytes in the list). The functions for searching a prefix have a worst-case
  complexity of /O(max(L,R))/. This means that the operation can become linear with
  /R/, the number of elements found for the prefix, with a minimum of /L/.

  This module has versions of the \"modifying\" operations,
  like insert, update, delete and map, that force evaluating
  the attribute values before doing the operation.
  All \"reading\" operations and the data types are reexported
  from Data.StringMap.Base.

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

        -- * Construction
        , empty
        , singleton

        -- ** Insertion
        , insert
        , insertWith
        , insertWithKey

        -- ** Delete\/Update
        , adjust
        , adjustWithKey
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
        
        -- ** Interset
        , intersection
        , intersectionWith

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
        , toListShortestFirst

        -- ** Maps
        , fromMap
        , toMap

        -- * Debugging
        , space
        , keyChars

        -- * Prefix and Fuzzy Search
        , prefixFilter     -- fuzzy search
        , prefixFilterNoCase
        , lookupNoCase

        )
where

import           Data.StringMap.Base        hiding (adjust, adjustWithKey,
                                             delete, fromList, insert,
                                             insertWith, insertWithKey, map,
                                             mapM, mapMaybe, mapWithKey,
                                             mapWithKeyM, singleton, union,
                                             unionWith, update, updateWithKey)
import qualified Data.StringMap.Base        as Base
import           Data.StringMap.FuzzySearch

import           Prelude                    hiding (lookup, map, mapM, null,
                                             succ)

--import Data.Strict.Tuple
import qualified Data.List                  as L
--import Data.BitUtil
--import Data.StrictPair

-- ----------------------------------------

normError               :: String -> a
normError               = normError' "Data.StringMap.Strict"

-- ----------------------------------------

-- | /O(1)/ Create a map with a single element.
--
-- the attribute value is evaluated to WHNF

singleton               :: Key -> a -> StringMap a
singleton k !v          = Base.singleton k v  -- anyseq (fromKey k) (val v empty)

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

-- | /O(n)/ Creates a trie from a list of key\/value pairs.

fromList                        :: [(Key, a)] -> StringMap a
fromList                        = L.foldl' (\p (k, v) -> insert k v p) empty

-- | /O(min(n,L))/ Updates a value at a given key (if that key is in the trie) or deletes the
-- element if the result of the updating function is 'Nothing'. If the key is not found, the trie
-- is returned unchanged.
-- The updated value is evaluated to WHNF before insertion.

update                          :: (a -> Maybe a) -> Key -> StringMap a -> StringMap a
update                          = update'

{-# INLINE update #-}

-- | /O(min(n,L))/ Updates a value at a given key (if that key is in the trie) or deletes the
-- element if the result of the updating function is 'Nothing'. If the key is not found, the trie
-- is returned unchanged.
--  The updated value is evaluated to WHNF before insertion.

updateWithKey                   :: (Key -> a -> Maybe a) -> Key -> StringMap a -> StringMap a
updateWithKey f k               = update' (f k) k

{-# INLINE updateWithKey #-}

-- | /O(min(n,L))/ Delete an element from the map. If no element exists for the key, the map
-- remains unchanged.

delete                          :: Key -> StringMap a -> StringMap a
delete                          = update' (const Nothing)

{-# INLINE delete #-}

adjust                          :: (a -> a) -> Key -> StringMap a -> StringMap a
adjust f                        = update' (Just . f)

{-# INLINE adjust #-}

adjustWithKey                   :: (Key -> a -> a) -> Key -> StringMap a -> StringMap a
adjustWithKey f k               = update' (Just . f k) k

{-# INLINE adjustWithKey #-}

-- | /O(n+m)/ Left-biased union of two maps. It prefers the first map when duplicate keys are
-- encountered, i.e. ('union' == 'unionWith' 'const').

union                           :: StringMap a -> StringMap a -> StringMap a
union                           = union' const

{-# INLINE union #-}

-- | /O(n+m)/ Union with a combining function.

unionWith                       :: (a -> a -> a) -> StringMap a -> StringMap a -> StringMap a
unionWith                       = union'

{-# INLINE unionWith #-}

-- | /O(n)/ Map a function over all values in the prefix tree.

map                             :: (a -> b) -> StringMap a -> StringMap b
map f                           = mapWithKey (const f)

{-# INLINE map #-}

mapWithKey                      :: (Key -> a -> b) -> StringMap a -> StringMap b
mapWithKey f                    = map' f id

{-# INLINE mapWithKey #-}

-- | /O(n)/ Updates a value or deletes the element,
-- if the result of the updating function is 'Nothing'.

mapMaybe                          :: (a -> Maybe b) -> StringMap a -> StringMap b
mapMaybe                          = mapMaybe'

{-# INLINE mapMaybe #-}

-- | Monadic map

mapM                            :: Monad m => (a -> m b) -> StringMap a -> m (StringMap b)
mapM f                          = mapWithKeyM (const f)

{-# INLINE mapM #-}

-- | Monadic mapWithKey

mapWithKeyM                     :: Monad m => (Key -> a -> m b) -> StringMap a -> m (StringMap b)
mapWithKeyM f                   = mapM'' f id

{-# INLINE mapWithKeyM #-}

-- ----------------------------------------
--
-- internal functions forcing evaluation of attribute values to WHNF
--
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

-- ----------------------------------------

update'                         :: (a -> Maybe a) -> Key -> StringMap a -> StringMap a
update' f k0                    = upd k0 . norm
    where
    upd'                        = update' f

    upd k (Branch c' s' n')
        = case k of
          []                    -> branch c' s' n'
          (c : k1)
              | c <  c'         -> branch c' s' n'
              | c == c'         -> branch c (upd' k1 s')            n'
              | otherwise       -> branch c'         s'     (upd' k n')

    upd _ Empty                 = empty

    upd k (Val v' t')
        = case k of
          []                    -> case f v' of
                                     Nothing   -> t'
                                     Just !v'' -> val v'' t'   -- force WHNF of new attr value
          _                     -> val v' (upd' k t')

    upd _ _                     = normError "update'"

-- ----------------------------------------

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

-- ----------------------------------------

-- map functions forcing evaluation of attr to WHNF

map'                            :: (Key -> a -> b) -> (Key -> Key) -> StringMap a -> StringMap b
map' _ _ (Empty)                = Empty
map' f k (Val v t)              = (Val      $! (f (k []) v))         (map' f k t)
map' f k (Branch c s n)         = Branch c  (map' f ((c :) . k)   s) (map' f k n)
map' f k (Leaf v)               = Leaf      $! (f (k []) v)
map' f k (Last c s)             = Last   c  (map' f ((c :)   . k) s)
map' f k (LsSeq cs s)           = LsSeq  cs (map' f ((toKey cs ++) . k) s)
map' f k (BrSeq cs s n)         = BrSeq  cs (map' f ((toKey cs ++) . k) s) (map' f k n)
map' f k (LsSeL cs v)           = LsSeL  cs $! (f (k []) v)
map' f k (BrSeL cs v n)         =(BrSeL  cs $! (f (k []) v))         (map' f k n)
map' f k (LsVal c  v)           = LsVal  c  $! (f (k []) v)
map' f k (BrVal c  v n)         =(BrVal  c  $! (f (k []) v))         (map' f k n)


mapMaybe'                       :: (a -> Maybe b) -> StringMap a -> StringMap b
mapMaybe' f                     = upd . norm
    where
    upd'                        = mapMaybe' f

    upd (Branch c' s' n')       = branch c' (upd' s') (upd' n')
    upd Empty                   = empty
    upd (Val v' t')             = case f v' of
                                    Nothing   -> t
                                    Just !v'' -> val v'' t      -- force WHNF for v
                                  where t = upd' t'
    upd _                       = normError "update'"


mapM''                          :: Monad m =>
                                   (Key -> a -> m b) -> (Key -> Key) -> StringMap a -> m (StringMap b)
mapM'' f k                      = mapn . norm
    where
    mapn'                       = mapM'' f

    mapn Empty                  = return $ empty
    mapn (Val v t)              = do
                                  !v' <- f (k []) v     -- force WHNF for v'
                                  t'  <- mapn' k t
                                  return $ val v' t'
    mapn (Branch c s n)         = do
                                  s' <- mapn' ((c :) . k) s
                                  n' <- mapn'          k  n
                                  return $ branch c s' n'
    mapn _                      = normError "mapM''"

-- ----------------------------------------

