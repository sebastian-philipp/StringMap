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

import Prelude hiding ( succ, lookup, map, mapM, null )

import Data.StringMap.Base
import Data.StringMap.FuzzySearch
import Data.StringMap.Types
