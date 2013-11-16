{-# LANGUAGE BangPatterns #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.Base
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

module Data.StringMap.Base
        (
        -- * Map type
          StringMap (Empty, Val, Branch)-- (..) I don't think we should export the constructors.
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
        , stat
        , keyChars

        -- Internal
        , cutPx'
        , cutAllPx'
        , branch
        , val
        , siseq
        , fromKey
        , norm
        , normError
        )
where

import           Prelude                  hiding (lookup, map, mapM, null, succ)

import           Control.Arrow
import           Control.DeepSeq

import qualified Data.Foldable            as F

import           Data.Binary
import qualified Data.List                as L
import qualified Data.Map                 as M
import           Data.Maybe               hiding (mapMaybe)

import           Data.StringMap.StringSet
import           Data.StringMap.Types

data StringMap v       = Empty
                        | Val    { value' ::   v
                                 , tree   :: ! (StringMap v)
                                 }
                        | Branch { sym    :: {-# UNPACK #-}
                                             ! Sym
                                 , child  :: ! (StringMap v)
                                 , next   :: ! (StringMap v)
                                 }

                        -- the space optimisation nodes, these
                        -- will be normalized during access into
                        -- the three constructors Empty, Val and Branch

                        | Leaf   { value' ::   v                -- a value at a leaf of the tree
                                 }
                        | Last   { sym    :: {-# UNPACK #-}
                                             ! Sym              -- the last entry in a branch list
                                 , child  :: ! (StringMap v)    -- or no branch but a single child
                                 }
                        | LsSeq  { syms  :: ! Key1              -- a sequence of single childs
                                 , child :: ! (StringMap v)     -- in a last node
                                 }
                        | BrSeq  { syms  :: ! Key1              -- a sequence of single childs
                                 , child :: ! (StringMap v)     -- in a branch node
                                 , next  :: ! (StringMap v)
                                 }
                        | LsSeL  { syms   :: ! Key1             -- a sequence of single childs
                                 , value' ::   v                -- with a leaf
                                 }
                        | BrSeL  { syms   :: ! Key1             -- a sequence of single childs
                                 , value' ::   v                -- with a leaf in a branch node
                                 , next   :: ! (StringMap v)
                                 }
                        | BrVal  { sym    :: {-# UNPACK #-}
                                             ! Sym              -- a branch with a single char
                                 , value' ::   v                -- and a value
                                 , next   :: ! (StringMap v)
                                 }
                        | LsVal  { sym    :: {-# UNPACK #-}
                                              ! Sym             -- a last node with a single char
                                 , value' ::   v                -- and a value
                                 }
                          deriving (Show, Eq, Ord)

-- | strict list of chars with unpacked fields
-- and packing of 2 or 3 chars into a single object
--
-- for internal use in prefix tree to optimize space efficiency

data Key1               = Nil
                        | S1 {-# UNPACK #-} ! Sym
                        | S2 {-# UNPACK #-} ! Sym  {-# UNPACK #-} ! Sym
                        | S3 {-# UNPACK #-} ! Sym  {-# UNPACK #-} ! Sym  {-# UNPACK #-} ! Sym
                        | C1 {-# UNPACK #-} ! Sym
                                            ! Key1
                        | C2 {-# UNPACK #-} ! Sym  {-# UNPACK #-} ! Sym
                                            ! Key1
                        | C3 {-# UNPACK #-} ! Sym  {-# UNPACK #-} ! Sym  {-# UNPACK #-} ! Sym
                                            ! Key1
                          deriving (Eq, Ord)

instance Show Key1 where
    show k = show (toKey k)

mk1                     :: Sym -> Key1
mk1 s1                  = S1 s1
-- mk1 s1                  = C1 s1 Nil

mk2                     :: Sym -> Sym -> Key1
mk2 s1 s2               = S2 s1 s2
-- mk2 s1 s2               = C1 s1 (mk1 s2)
-- mk2 s1 s2               = C1 s1 (C1 s2 Nil)

mk3                     :: Sym -> Sym -> Sym -> Key1
mk3 s1 s2 s3            = S3 s1 s2 s3

cons1                   :: Sym -> Key1 -> Key1
cons1 s Nil             = mk1 s
cons1 s (S1 s2)         = mk2 s s2
cons1 s (S2 s2 s3)      = mk3 s s2 s3
cons1 s (C1 s2 k2)      = C2 s s2 k2
cons1 s (C2 s2 s3 k3)   = C3 s s2 s3 k3
cons1 s k               = C1 s    k

uncons1                 :: Key1 -> (Sym, Key1)
uncons1 (S1 s)          = (s, Nil)
uncons1 (S2 s s2)       = (s, mk1 s2)
uncons1 (S3 s s2 s3)    = (s, mk2 s2 s3)
uncons1 (C1 s k1)       = (s, k1)
uncons1 (C2 s s2 k1)    = (s, C1 s2 k1)
uncons1 (C3 s s2 s3 k1) = (s, C2 s2 s3 k1)
uncons1 Nil             = error "uncons1 with Nil"

{-# INLINE mk1 #-}
{-# INLINE mk2 #-}
{-# INLINE mk3 #-}
{-# INLINE cons1 #-}
{-# INLINE uncons1 #-}


toKey                   :: Key1 -> Key
toKey (C3 s1 s2 s3 k)   = s1 : s2 : s3 : toKey k
toKey (C2 s1 s2    k)   = s1 : s2      : toKey k
toKey (C1 s1       k)   = s1           : toKey k
toKey (S3 s1 s2 s3)     = s1 : s2 : s3 : []
toKey (S2 s1 s2   )     = s1 : s2      : []
toKey (S1 s1      )     = s1           : []
toKey Nil               = []

fromKey                 :: Key -> Key1
fromKey k1               = foldr cons1 Nil k1

length1                 :: Key1 -> Int
length1                 = length . toKey

space1                  :: Key1 -> Int
space1 Nil              = 0
space1 (S1 _)           = 2
space1 (S2 _ _)         = 3
space1 (S3 _ _ _)       = 4
space1 (C1 _ k1)        = 3 + space1 k1
space1 (C2 _ _ k1)      = 4 + space1 k1
space1 (C3 _ _ _ k1)    = 5 + space1 k1

-- ----------------------------------------

-- smart constructors

empty                           :: StringMap v
empty                           = Empty

{-# INLINE empty #-}

val                             :: v -> StringMap v -> StringMap v
val v Empty                     = Leaf v
val v t                         = Val v t

{-# INLINE val #-}

branch                          :: Sym -> StringMap v -> StringMap v -> StringMap v
branch !_k Empty        n       = n

branch !k (Leaf   v   ) Empty   = LsVal  k     v
branch !k (LsVal  k1 v) Empty   = LsSeL (mk2 k k1) v
branch !k (LsSeL  ks v) Empty   = LsSeL (cons1 k ks) v
branch !k (Last   k1 c) Empty   = lsseq (mk2 k k1) c
branch !k (LsSeq  ks c) Empty   = lsseq (cons1 k ks) c
branch !k            c  Empty   = Last k c

branch !k (Leaf   v   ) n       = BrVal  k     v n
branch !k (LsVal  k1 v) n       = BrSeL (mk2 k k1) v n
branch !k (LsSeL  ks v) n       = BrSeL (cons1 k ks) v n
branch !k (Last   k1 c) n       = brseq (mk2 k k1) c n
branch !k (LsSeq  ks c) n       = brseq (cons1 k ks) c n
branch !k            c  n       = Branch k c n

lsseq                           :: Key1 -> StringMap v -> StringMap v
lsseq !k (Leaf v)               = LsSeL k v
lsseq !k c                      = LsSeq k c

{-# INLINE lsseq #-}

brseq                           :: Key1 -> StringMap v -> StringMap v -> StringMap v
brseq !k (Leaf v) n             = BrSeL k v n
brseq !k c        n             = BrSeq k c n

{-# INLINE brseq #-}

siseq                           :: Key1 -> StringMap v -> StringMap v
siseq Nil   c                   = c
siseq k     c                   = case uncons1 k of
                                    (k1, Nil) -> Last  k1 c
                                    _         -> LsSeq k  c
{-# INLINE siseq #-}

-- smart selectors

norm                            :: StringMap v -> StringMap v
norm (Leaf v)                   = Val v empty
norm (Last k c)                 = Branch k c empty
norm (LsSeq k' c)               = case uncons1 k' of
                                    (k, Nil) -> Branch k c            empty
                                    (k, ks)  -> Branch k (siseq ks c) empty
norm (BrSeq k' c n)             = case uncons1 k' of
                                    (k, Nil) -> Branch k c            n
                                    (k, ks)  -> Branch k (siseq ks c) n
norm (LsSeL    ks  v)           = norm (LsSeq ks  (val v empty))
norm (BrSeL    ks  v n)         = norm (BrSeq ks  (val v empty) n)
norm (LsVal    k   v)           = norm (LsSeq (mk1 k) (val v empty))
norm (BrVal    k   v n)         = norm (BrSeq (mk1 k) (val v empty) n)
norm t                          = t

-- ----------------------------------------

deepNorm                :: StringMap v -> StringMap v
deepNorm t0
    = case norm t0 of
      Empty             -> Empty
      Val v t           -> Val v (deepNorm t)
      Branch c s n      -> Branch c (deepNorm s) (deepNorm n)
      _                 -> normError "deepNorm"

-- ----------------------------------------

normError               :: String -> a
normError f             = error (f ++ ": pattern match error, prefix tree not normalized")

-- ----------------------------------------

-- | /O(1)/ Is the map empty?

null                    :: StringMap a -> Bool
null Empty              = True
null _                  = False

{-# INLINE null #-}

-- | /O(1)/ Create a map with a single element.

singleton               :: Key -> a -> StringMap a
singleton k v           = siseq (fromKey k) (val v empty)

{-# INLINE singleton #-}

-- | /O(1)/ Extract the value of a node (if there is one)

value                   :: Monad m => StringMap a -> m a
value t                 = case norm t of
                          Val v _       -> return v
                          _             -> fail "StringMap.value: no value at this node"

{-# INLINE value #-}

-- | /O(1)/ Extract the value of a node or return a default value if no value exists.

valueWithDefault        :: a -> StringMap a -> a
valueWithDefault d t    = fromMaybe d . value $ t


-- | /O(1)/ Extract the successors of a node

succ                    :: StringMap a -> StringMap a
succ t                  = case norm t of
                          Val _ t'      -> succ t'
                          t'            -> t'
{-# INLINE succ #-}

-- ----------------------------------------

-- | /O(min(n,L))/ Find the value associated with a key. The function will @return@ the result in
-- the monad or @fail@ in it if the key isn't in the map.

lookup                          :: Monad m => Key -> StringMap a -> m a
lookup k t                      = case lookup' k t of
                                  Just v  -> return v
                                  Nothing -> fail "StringMap.lookup: Key not found"
{-# INLINE lookup #-}

-- | /O(min(n,L))/ Find the value associated with a key. The function will @return@ the result in
-- the monad or @fail@ in it if the key isn't in the map.

findWithDefault                 :: a -> Key -> StringMap a -> a
findWithDefault v0 k            = fromMaybe v0 . lookup' k

{-# INLINE findWithDefault #-}

-- | /O(min(n,L))/ Is the key a member of the map?

member                          :: Key -> StringMap a -> Bool
member k                        = isJust . lookup k

{-# INLINE member #-}

-- | /O(min(n,L))/ Find the value at a key. Calls error when the element can not be found.

(!)                             :: StringMap a -> Key -> a
(!)                             = flip $ findWithDefault (error "StringMap.! : element not in the map")

-- | /O(min(n,L))/ Insert a new key and value into the map. If the key is already present in
-- the map, the associated value will be replaced with the new value.

insert                          :: Key -> a -> StringMap a -> StringMap a
insert                          = insertWith const

{-# INLINE insert #-}

-- | /O(min(n,L))/ Insert with a combining function. If the key is already present in the map,
-- the value of @f new_value old_value@ will be inserted.

insertWith                      :: (a -> a -> a) -> Key -> a -> StringMap a -> StringMap a
insertWith f                    = flip $ insert' f

{-# INLINE insertWith #-}

-- | /O(min(n,L))/ Insert with a combining function. If the key is already present in the map,
-- the value of @f key new_value old_value@ will be inserted.

insertWithKey                   :: (Key -> a -> a -> a) -> Key -> a -> StringMap a -> StringMap a
insertWithKey f k               = insertWith (f k) k


{-# INLINE insertWithKey #-}

-- | /O(min(n,L))/ Updates a value at a given key (if that key is in the trie) or deletes the
-- element if the result of the updating function is 'Nothing'. If the key is not found, the trie
-- is returned unchanged.

update                          :: (a -> Maybe a) -> Key -> StringMap a -> StringMap a
update                          = update'

{-# INLINE update #-}

-- | /O(min(n,L))/ Updates a value at a given key (if that key is in the trie) or deletes the
-- element if the result of the updating function is 'Nothing'. If the key is not found, the trie
-- is returned unchanged.

updateWithKey                   :: (Key -> a -> Maybe a) -> Key -> StringMap a -> StringMap a
updateWithKey f k               = update' (f k) k

{-# INLINE updateWithKey #-}

-- | /O(min(n,L))/ Delete an element from the map. If no element exists for the key, the map
-- remains unchanged.

delete                          :: Key -> StringMap a -> StringMap a
delete                          = update' (const Nothing)

{-# INLINE delete #-}

-- ----------------------------------------

lookupPx'                       :: Key -> StringMap a -> StringMap a
lookupPx' k0                    = look k0 . norm
    where
    look [] t                   = t
    look k@(c : k1) (Branch c' s' n')
        | c <  c'               = empty
        | c == c'               = lookupPx' k1 s'
        | otherwise             = lookupPx' k  n'
    look _          Empty       = empty
    look k         (Val _v' t') = lookupPx' k t'

    look _ _                    = normError "lookupPx'"

-- Internal lookup function which is generalised for arbitrary monads above.

lookup'                         :: Key -> StringMap a -> Maybe a
lookup' k t
    = case lookupPx' k t of
      Val v _                   -> Just v
      _                         -> Nothing

-- ----------------------------------------

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key.

prefixFind                      :: Key -> StringMap a -> [a]
prefixFind k                    = elems . lookupPx' k

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key and include the keys
-- in the result.

prefixFindWithKey               :: Key -> StringMap a -> [(Key, a)]
prefixFindWithKey k             = fmap (first (k ++)) . toList . lookupPx' k

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
          []                    -> val (f v v') t'
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
          []                    -> maybe t' (flip val t') $ f v'
          _                     -> val v' (upd' k t')
    upd _ _                     = normError "update'"

-- ----------------------------------------

-- | /O(n+m)/ Left-biased union of two maps. It prefers the first map when duplicate keys are
-- encountered, i.e. ('union' == 'unionWith' 'const').

union                                           :: StringMap a -> StringMap a -> StringMap a
union                                           = union' const

-- | /O(n+m)/ Union with a combining function.

unionWith                                       :: (a -> a -> a) -> StringMap a -> StringMap a -> StringMap a
unionWith                                       = union'

union'                                          :: (a -> a -> a) -> StringMap a -> StringMap a -> StringMap a
union' f pt1 pt2                                = uni (norm pt1) (norm pt2)
    where
    uni' t1' t2'                                = union' f (norm t1') (norm t2')

    uni     Empty                Empty          = empty
    uni     Empty               (Val v2 t2)     = val v2 t2
    uni     Empty               (Branch c2 s2 n2)
                                                = branch c2 s2 n2

    uni    (Val v1 t1)           Empty          = val    v1     t1
    uni    (Val v1 t1)          (Val v2 t2)     = val (f v1 v2) (uni' t1 t2)
    uni    (Val v1 t1)       t2@(Branch _ _ _)  = val    v1     (uni' t1 t2)

    uni    (Branch c1 s1 n1)     Empty          = branch c1 s1 n1
    uni t1@(Branch _  _  _ )    (Val v2 t2)     = val v2 (uni' t1 t2)
    uni t1@(Branch c1 s1 n1) t2@(Branch c2 s2 n2)
        | c1 <  c2                              = branch c1       s1     (uni' n1 t2)
        | c1 >  c2                              = branch c2          s2  (uni' t1 n2)
        | otherwise                             = branch c1 (uni' s1 s2) (uni' n1 n2)
    uni _                    _                  = normError "union'"

-- ----------------------------------------

-- | /O(n+m)/ Union with a combining function, including the key.

unionWithKey                                    :: (Key -> a -> a -> a) -> StringMap a -> StringMap a -> StringMap a
unionWithKey f                                  = union'' f id

union''                                         :: (Key -> a -> a -> a) -> (Key -> Key) -> StringMap a -> StringMap a -> StringMap a
union'' f kf pt1 pt2                            = uni (norm pt1) (norm pt2)
    where
    uni' t1' t2'                                = union'' f kf (norm t1') (norm t2')

    uni     Empty                Empty          = empty
    uni     Empty               (Val v2 t2)     = val v2 t2
    uni     Empty               (Branch c2 s2 n2)
                                                = branch c2 s2 n2

    uni    (Val v1 t1)           Empty          = val            v1           t1
    uni    (Val v1 t1)          (Val v2 t2)     = val (f (kf []) v1 v2) (uni' t1 t2)
    uni    (Val v1 t1)       t2@(Branch _ _ _)  = val            v1     (uni' t1 t2)

    uni    (Branch c1 s1 n1)     Empty          = branch c1 s1 n1
    uni t1@(Branch _  _  _ )    (Val v2 t2)     = val v2 (uni' t1 t2)
    uni t1@(Branch c1 s1 n1) t2@(Branch c2 s2 n2)
        | c1 <  c2                              = branch c1                         s1     (uni' n1 t2)
        | c1 >  c2                              = branch c2                            s2  (uni' t1 n2)
        | otherwise                             = branch c1 (union'' f (kf . (c1:)) s1 s2) (uni' n1 n2)

    uni _                    _                  = normError "union''"

-- ----------------------------------------
--
-- | /(O(min(n,m))/ Difference between two tries (based on keys).

difference                      :: StringMap a -> StringMap b -> StringMap a
difference                      = differenceWith (const (const Nothing))

-- | /(O(min(n,m))/ Difference with a combining function. If the combining function always returns
-- 'Nothing', this is equal to proper set difference.

differenceWith                  :: (a -> b -> Maybe a) -> StringMap a -> StringMap b -> StringMap a
differenceWith f                = differenceWithKey (const f)

-- | /O(min(n,m))/ Difference with a combining function, including the key. If two equal keys are
-- encountered, the combining function is applied to the key and both values. If it returns
-- 'Nothing', the element is discarded, if it returns 'Just' a value, the element is updated
-- with the new value.

differenceWithKey               :: (Key -> a -> b -> Maybe a) -> StringMap a -> StringMap b -> StringMap a
differenceWithKey f             = diff'' f id

diff''                          :: (Key -> a -> b -> Maybe a) ->
                                   (Key -> Key) ->
                                   StringMap a -> StringMap b -> StringMap a
diff'' f kf pt1 pt2             = dif (norm pt1) (norm pt2)
    where
    dif' t1' t2'                = diff'' f kf (norm t1') (norm t2')

    dif     Empty               _               = empty

    dif    (Val v1 t1)           Empty          = val  v1       t1
    dif    (Val v1 t1)          (Val v2 t2)     =
        case f (kf []) v1 v2 of
                             Nothing            ->         dif' t1 t2
                             Just nv            -> val nv (dif' t1 t2)
    dif    (Val v1 t1)       t2@(Branch _ _ _)  =  val v1 (dif' t1 t2)

    dif    (Branch c1 s1 n1)     Empty          = branch c1 s1 n1
    dif t1@(Branch _  _  _ )    (Val _  t2)     = dif' t1 t2
    dif t1@(Branch c1 s1 n1) t2@(Branch c2 s2 n2)
        | c1 <  c2                              = branch c1                        s1       (dif' n1 t2)
        | c1 >  c2                              =                                            dif' t1 n2
        | otherwise                             = branch c1 (diff'' f (kf . (c1:)) s1 s2)   (dif' n1 n2)
    dif _                    _                  = normError "diff''"


-- ----------------------------------------

-- | cut off all branches from a tree @t2@ that are not part of set @t1@
--
-- the following laws must holds
--
-- @lookup' k' . cutPx' (singlePS k) $ t == lookup' k t@ for every @k'@ with @k@ prefix of @k'@
--
-- @lookup' k' . cutPx' (singlePS k) $ t == Nothing@ for every @k'@ with @k@ not being a prefix of @k'@

cutPx''                         :: (StringMap a -> StringMap a) -> StringSet -> StringMap a -> StringMap a
cutPx'' cf s1' t2'              = cut s1' (norm t2')
    where
    cut     PSempty           _t2               = empty
    cut    (PSelem _s1)       t2                = cf t2
    cut    (PSnext _  _  _ )  Empty             = empty
    cut t1@(PSnext _  _  _ ) (Val _ t2)         = cut t1 (norm t2)
    cut t1@(PSnext c1 s1 n1) t2@(Branch c2 s2 n2)
        | c1 <  c2                              = cut n1 t2
        | c1 >  c2                              = cut t1 (norm n2)
        | otherwise                             = branch c1 (cutPx'' cf s1 s2) (cutPx'' cf n1 n2)
    cut _                    _                  = normError "cutPx''"

cutPx'                          :: StringSet -> StringMap a -> StringMap a
cutPx'                          = cutPx'' id

cutAllPx'                       :: StringSet -> StringMap a -> StringMap a
cutAllPx'                       = cutPx'' (cv . norm)
    where
    cv (Val v _)                = val v empty
    cv _                        = empty

-- ----------------------------------------

-- | /O(n)/ Map a function over all values in the prefix tree.

map                             :: (a -> b) -> StringMap a -> StringMap b
map f                           = mapWithKey (const f)


mapWithKey                      :: (Key -> a -> b) -> StringMap a -> StringMap b
mapWithKey f                    = map' f id


map'                            :: (Key -> a -> b) -> (Key -> Key) -> StringMap a -> StringMap b
map' _ _ (Empty)                = Empty
map' f k (Val v t)              = Val       (f (k []) v)             (map' f k t)
map' f k (Branch c s n)         = Branch c  (map' f ((c :) . k)   s) (map' f k n)
map' f k (Leaf v)               = Leaf      (f (k []) v)
map' f k (Last c s)             = Last   c  (map' f ((c :)   . k) s)
map' f k (LsSeq cs s)           = LsSeq  cs (map' f ((toKey cs ++) . k) s)
map' f k (BrSeq cs s n)         = BrSeq  cs (map' f ((toKey cs ++) . k) s) (map' f k n)
map' f k (LsSeL cs v)           = LsSeL  cs (f (k []) v)
map' f k (BrSeL cs v n)         = BrSeL  cs (f (k []) v)             (map' f k n)
map' f k (LsVal c  v)           = LsVal  c  (f (k []) v)
map' f k (BrVal c  v n)         = BrVal  c  (f (k []) v)             (map' f k n)

-- | /O(n)/ Updates a value or deletes the element if the result of the updating function is 'Nothing'.

mapMaybe                          :: (a -> Maybe b) -> StringMap a -> StringMap b
mapMaybe                          = mapMaybe'

{-# INLINE mapMaybe #-}

mapMaybe'                       :: (a -> Maybe b) -> StringMap a -> StringMap b
mapMaybe' f                     = upd . norm
    where
    upd'                        = mapMaybe' f
    upd (Branch c' s' n')       = branch c' (upd' s') (upd' n')
    upd Empty                   = empty
    upd (Val v' t')             = maybe t (flip val t) $ f v'
        where t = upd' t'
    upd _                       = normError "update'"
-- ----------------------------------------
{- not yet used

-- | Variant of map that works on normalized trees

mapN                            :: (a -> b) -> StringMap a -> StringMap b
mapN f                          = mapWithKeyN (const f)


mapWithKeyN                     :: (Key -> a -> b) -> StringMap a -> StringMap b
mapWithKeyN f                   = map'' f id

map''                           :: (Key -> a -> b) -> (Key -> Key) -> StringMap a -> StringMap b
map'' f k                       = mapn . norm
    where
    mapn Empty                  = empty
    mapn (Val v t)              = val (f (k []) v) (map'' f k t)
    mapn (Branch c s n)         = branch c (map'' f ((c :) . k) s) (map'' f k n)
    mapn _                      = normError "map''"
-- -}

-- ----------------------------------------

-- | Monadic map

mapM                            :: Monad m => (a -> m b) -> StringMap a -> m (StringMap b)
mapM f                          = mapWithKeyM (const f)

-- | Monadic mapWithKey

mapWithKeyM                     :: Monad m => (Key -> a -> m b) -> StringMap a -> m (StringMap b)
mapWithKeyM f                   = mapM'' f id

mapM''                          :: Monad m => (Key -> a -> m b) -> (Key -> Key) -> StringMap a -> m (StringMap b)
mapM'' f k                      = mapn . norm
    where
    mapn Empty                  = return $ empty
    mapn (Val v t)              = do
                                  v' <- f (k []) v
                                  t' <- mapM'' f k t
                                  return $ val v' t'
    mapn (Branch c s n)         = do
                                  s' <- mapM'' f ((c :) . k) s
                                  n' <- mapM'' f          k  n
                                  return $ branch c s' n'
    mapn _                      = normError "mapM''"

-- ----------------------------------------
--
-- A prefix tree visitor

data StringMapVisitor a b      = PTV
    { v_empty  :: b
    , v_val    :: a    -> b -> b
    , v_branch :: Sym  -> b -> b -> b
    , v_leaf   :: a    -> b
    , v_last   :: Sym  -> b -> b
    , v_lsseq  :: Key1 -> b -> b
    , v_brseq  :: Key1 -> b -> b -> b
    , v_lssel  :: Key1 -> a -> b
    , v_brsel  :: Key1 -> a -> b -> b
    , v_lsval  :: Sym  -> a -> b
    , v_brval  :: Sym  -> a -> b -> b
    }

visit                   :: StringMapVisitor a b -> StringMap a -> b

visit v (Empty)         = v_empty  v
visit v (Val v' t)      = v_val    v v' (visit v t)
visit v (Branch c s n)  = v_branch v c  (visit v s) (visit v n)
visit v (Leaf v')       = v_leaf   v v'
visit v (Last c s)      = v_last   v c  (visit v s)
visit v (LsSeq cs s)    = v_lsseq  v cs (visit v s)
visit v (BrSeq cs s n)  = v_brseq  v cs (visit v s) (visit v n)
visit v (LsSeL cs v')   = v_lssel  v cs v'
visit v (BrSeL cs v' n) = v_brsel  v cs v'          (visit v n)
visit v (LsVal c  v')   = v_lsval  v c  v'
visit v (BrVal c  v' n) = v_brval  v c  v'          (visit v n)

-- ----------------------------------------
--
-- | space required by a prefix tree (logically)
--
-- Singletons are counted as 0, all other n-ary constructors
-- are counted as n+1 (1 for the constructor and 1 for every field)
-- cons nodes of char lists are counted 2, 1 for the cons, 1 for the char
-- for values only the ref to the value is counted, not the space for the value itself
-- key chars are assumed to be unboxed

space                   :: StringMap a -> Int
space                   = visit $
                          PTV
                          { v_empty             = 0
                          , v_val               = const (3 +)
                          , v_branch            = const $ \ s n -> 4 + s + n
                          , v_leaf              = const 2
                          , v_last              = const (3 +)
                          , v_lsseq             = \ cs s   ->     space1 cs + s
                          , v_brseq             = \ cs s n -> 4 + space1 cs + s + n
                          , v_lssel             = \ cs _   -> 3 + space1 cs
                          , v_brsel             = \ cs _ n -> 4 + space1 cs     + n
                          , v_lsval             = \ _  _   -> 3
                          , v_brval             = \ _  _ n -> 4                 + n
                          }

keyChars                :: StringMap a -> Int
keyChars                = visit $
                          PTV
                          { v_empty             = 0
                          , v_val               = \ _  t   -> t
                          , v_branch            = \ _  s n -> 1 + s + n
                          , v_leaf              = \ _      -> 0
                          , v_last              = \ _  s   -> 1 + s
                          , v_lsseq             = \ cs s   -> length1 cs + s
                          , v_brseq             = \ cs s n -> length1 cs + s + n
                          , v_lssel             = \ cs _   -> length1 cs
                          , v_brsel             = \ cs _ n -> length1 cs     + n
                          , v_lsval             = \ _  _   -> 1
                          , v_brval             = \ _  _ n -> 1             + n
                          }

-- ----------------------------------------
--
-- | statistics about the # of different nodes in an optimized prefix tree

stat                    :: StringMap a -> StringMap Int
stat                    =  visit $
                          PTV
                          { v_empty             =             singleton "empty"  1
                          , v_val               = \ _  t   -> singleton "val"    1
                                                              `add` t
                          , v_branch            = \ _  s n -> singleton "branch" 1
                                                              `add` (s `add` n)
                          , v_leaf              = \ _      -> singleton "leaf"   1
                          , v_last              = \ _  s   -> singleton "last"   1
                                                              `add` s
                          , v_lsseq             = \ cs s   -> singleton (show' "lsseq" cs) 1
                                                              `add` singleton "size-key1" (length1 cs)
                                                              `add` singleton "space-key1" (space1 cs)
                                                              `add` s
                          , v_brseq             = \ cs s n -> singleton (show' "brseq" cs) 1
                                                              `add` singleton "size-key1" (length1 cs)
                                                              `add` singleton "space-key1" (space1 cs)
                                                              `add` (s `add` n)
                          , v_lssel             = \ cs _   -> singleton (show' "lssel" cs) 1
                                                              `add` singleton "size-key1" (length1 cs)
                                                              `add` singleton "space-key1" (space1 cs)
                          , v_brsel             = \ cs _ n -> singleton (show' "brseq" cs) 1
                                                              `add` singleton "size-key1" (length1 cs)
                                                              `add` singleton "space-key1" (space1 cs)
                                                              `add` n
                          , v_lsval             = \ _  _   -> singleton "lsval" 1
                          , v_brval             = \ _  _ n -> singleton "brval" 1
                                                              `add`          n
                          }
    where
    add                 = unionWith (+)
    show' c k1          = c ++ "-" ++ show (length1 k1)

-- ----------------------------------------

-- | /O(n)/ Fold over all key\/value pairs in the map.

foldWithKey                     :: (Key -> a -> b -> b) -> b -> StringMap a -> b
foldWithKey f e                 = fold' f e id

{-# INLINE foldWithKey #-}

-- | /O(n)/ Fold over all values in the map.

fold :: (a -> b -> b) -> b -> StringMap a -> b
fold f = foldWithKey $ const f

{-# INLINE fold #-}

{- not yet used

foldTopDown                     :: (Key -> a -> b -> b) -> b -> (Key -> Key) -> StringMap a -> b
foldTopDown f r k0              = fo k0 . norm
    where
    fo kf (Branch c' s' n')     = let r' = foldTopDown f r ((c' :) . kf) s' in foldTopDown f r' kf n'
    fo _ (Empty)                = r
    fo kf (Val v' t')           = let r' = f (kf []) v' r                   in foldTopDown f r' kf t'
    fo _  _                     = normError "foldTopDown"
-- -}

fold'                           :: (Key -> a -> b -> b) -> b -> (Key -> Key) -> StringMap a -> b
fold' f r k0                    = fo k0 . norm
    where
    fo kf (Branch c' s' n')     = let r' = fold' f r kf n' in fold' f r' (kf . (c':)) s'
    fo _  (Empty)               = r
    fo kf (Val v' t')           = let r' = fold' f r kf t' in f (kf []) v' r'
    fo _  _                     = normError "fold'"

-- | /O(n)/ Convert into an ordinary map.

toMap                           :: StringMap a -> M.Map Key a
toMap                           = foldWithKey M.insert M.empty

-- | /O(n)/ Convert an ordinary map into a Prefix tree

fromMap                         :: M.Map Key a -> StringMap a
fromMap                         = M.foldrWithKey insert empty

-- | /O(n)/ Returns all elements as list of key value pairs,

toList                          :: StringMap a -> [(Key, a)]
toList                          = foldWithKey (\k v r -> (k, v) : r) []

-- | /O(n)/ Creates a trie from a list of key\/value pairs.
fromList                        :: [(Key, a)] -> StringMap a
fromList                        = L.foldl' (\p (k, v) -> insert k v p) empty

-- | /O(n)/ The number of elements.
size                            :: StringMap a -> Int
size                            = fold (const (+1)) 0

-- | /O(n)/ Returns all values.
elems                           :: StringMap a -> [a]
elems                           = fold (:) []

-- | /O(n)/ Returns all values.
keys                            :: StringMap a -> [Key]
keys                            = foldWithKey (\ k _v r -> k : r) []

-- ----------------------------------------

-- | returns all key-value pairs in breadth first order (short words first)
-- this enables prefix search with upper bounds on the size of the result set
-- e.g. @ search ... >>> toListBF >>> take 1000 @ will give the 1000 shortest words
-- found in the result set and will ignore all long words
--
-- toList is derived from the following code found in the net when searching haskell breadth first search
--
-- Haskell Standard Libraray Implementation
--
-- > br :: Tree a -> [a]
-- > br t = map rootLabel $
-- >        concat $
-- >        takeWhile (not . null) $
-- >        iterate (concatMap subForest) [t]

toListBF                        :: StringMap v -> [(Key, v)]
toListBF                        = (\ t0 -> [(id, t0)])
                                  >>>
                                  iterate (concatMap (second norm >>> uncurry subForest))
                                  >>>
                                  takeWhile (not . L.null)
                                  >>>
                                  concat
                                  >>>
                                  concatMap (second norm >>> uncurry rootLabel)

rootLabel                       :: (Key -> Key) -> StringMap v -> [(Key, v)]
rootLabel kf (Val v _)          = [(kf [], v)]
rootLabel _  _                  = []

subForest                       :: (Key -> Key) -> StringMap v -> [(Key -> Key, StringMap v)]
subForest kf (Branch c s n)     = (kf . (c:), s) : subForest kf (norm n)
subForest _  Empty              = []
subForest kf (Val _ t)          = subForest kf (norm t)
subForest _  _                  = error "StringMap.Base.subForest: Pattern match failure"

-- ----------------------------------------

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key and include the keys
-- in the result. The result list contains short words first

prefixFindWithKeyBF             :: Key -> StringMap a -> [(Key, a)]
prefixFindWithKeyBF k           = fmap (first (k ++)) . toListBF . lookupPx' k

-- ----------------------------------------

instance Functor StringMap where
  fmap = map

instance F.Foldable StringMap where
  foldr = fold

{- for debugging not yet enabled

instance Show a => Show (StringMap a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

-- -}

-- ----------------------------------------

instance Read a => Read (StringMap a) where
  readsPrec p = readParen (p > 10) $
    \ r -> do
           ("fromList",s) <- lex r
           (xs,t) <- reads s
           return (fromList xs,t)

-- ----------------------------------------

instance NFData a => NFData (StringMap a) where
    rnf (Empty)         = ()
    rnf (Val v t)       = rnf v `seq` rnf t
    rnf (Branch _c s n) = rnf s `seq` rnf n
    rnf (Leaf v)        = rnf v
    rnf (Last _c s)     = rnf s
    rnf (LsSeq _ks s)   = rnf s
    rnf (BrSeq _ks s n) = rnf s `seq` rnf n
    rnf (LsSeL _ks v)   = rnf v
    rnf (BrSeL _ks v n) = rnf v `seq` rnf n
    rnf (LsVal k  v)    = rnf k `seq` rnf v
    rnf (BrVal k  v n)  = rnf k `seq` rnf v `seq` rnf n

-- ----------------------------------------
--
-- Provide native binary serialization (not via to-/fromList).

instance (Binary a) => Binary (StringMap a) where
    put (Empty)         = put (0::Word8)
    put (Val v t)       = put (1::Word8)  >> put v >> put t
    put (Branch c s n)  = put (2::Word8)  >> put c >> put s >> put n
    put (Leaf v)        = put (3::Word8)  >> put v
    put (Last c s)      = put (4::Word8)  >> put c >> put s
    put (LsSeq k s)     = put (5::Word8)  >> put (toKey k) >> put s
    put (BrSeq k s n)   = put (6::Word8)  >> put (toKey k) >> put s >> put n
    put (LsSeL k v)     = put (7::Word8)  >> put (toKey k) >> put v
    put (BrSeL k v n)   = put (8::Word8)  >> put (toKey k) >> put v >> put n
    put (LsVal k v)     = put (9::Word8)  >> put k >> put v
    put (BrVal k v n)   = put (10::Word8) >> put k >> put v >> put n

    get = do
          !tag <- getWord8
          case tag of
                   0 -> return Empty
                   1 -> do
                        !v <- get
                        !t <- get
                        return $! Val v t
                   2 -> do
                        !c <- get
                        !s <- get
                        !n <- get
                        return $! Branch c s n
                   3 -> do
                        !v <- get
                        return $! Leaf v
                   4 -> do
                        !c <- get
                        !s <- get
                        return $! Last c s
                   5 -> do
                        !k <- get
                        !s <- get
                        return $! LsSeq (fromKey k) s
                   6 -> do
                        !k <- get
                        !s <- get
                        !n <- get
                        return $! BrSeq (fromKey k) s n
                   7 -> do
                        !k <- get
                        !v <- get
                        return $! LsSeL (fromKey k) v
                   8 -> do
                        !k <- get
                        !v <- get
                        !n <- get
                        return $! BrSeL (fromKey k) v n
                   9 -> do
                        !k <- get
                        !v <- get
                        return $! LsVal k v
                   10 -> do
                        !k <- get
                        !v <- get
                        !n <- get
                        return $! BrVal k v n
                   _ -> fail "StringMap.get: error while decoding StringMap"

-- ----------------------------------------
