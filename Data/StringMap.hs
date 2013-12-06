{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap
  Copyright  : Copyright (C) 2009-2012 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: not portable

  Facade for prefix tree implementation

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

import           Prelude                    hiding (lookup, map, mapM, null,
                                             succ)

import           Data.StringMap.Base
import           Data.StringMap.FuzzySearch
import           Data.StringMap.Types

-- ----------------------------------------
