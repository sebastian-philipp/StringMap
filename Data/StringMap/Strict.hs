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

import           Data.StringMap.Base
import           Data.StringMap.FuzzySearch
import           Prelude                    hiding (lookup, map, mapM, null,
                                             succ)
