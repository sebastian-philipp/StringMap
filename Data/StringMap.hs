{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap
  Copyright  : Copyright (C) 2009-2013 Uwe Schmidt, Sebastian Philipp
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de), Sebastian Philipp
  Stability  : experimental
  Portability: not portable

  An efficient implementation of maps from strings to arbitrary values.

  Values can be associated with an arbitrary [Char] key. Searching for keys is very fast.
  The main differences to Data.Map and Data.IntMap are the special
  'prefixFind' functions, which can be used to perform prefix queries. The interface is
  heavily borrowed from "Data.Map" and "Data.IntMap".

  Most other function names clash with "Prelude" names, therefore this module is usually
  imported @qualified@, e.g.

  > import           Data.StringMap (StringMap)
  > import qualified Data.StringMap as M

  Many functions have a worst-case complexity of /O(min(n,L))/. This means that the operation
  can become linear with the number of elements with a maximum of /L/, the length of the
  key (the number of bytes in the list). The functions for searching a prefix have a worst-case
  complexity of /O(max(L,R))/. This means that the operation can become linear with
  /R/, the number of elements found for the prefix, with a minimum of /L/.

  This module reexports all generally usefull types and operations
  from Data.StringMap.Base.

-}

-- ----------------------------------------------------------------------------

module Data.StringMap
    (
    -- * Map type
    StringMap() -- Don't export the constructors
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
    , foldr
    , foldrWithKey

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

    -- * Prefix and Fuzzy Search
    , prefixFilter     -- fuzzy search
    , prefixFilterNoCase
    , lookupNoCase
    )
where

import           Prelude                    hiding (foldr, lookup, map, mapM, null,
                                             succ)

import           Data.StringMap.Base
import           Data.StringMap.FuzzySearch
import           Data.StringMap.Types

-- ----------------------------------------
