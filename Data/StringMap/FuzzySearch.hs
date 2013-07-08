{-# LANGUAGE BangPatterns #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.FuzzySearch
  Copyright  : Copyright (C) 2009-2012 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: not portable

  Functions for fuzzy search in a prefix tree

-}

-- ----------------------------------------------------------------------------

module Data.StringMap.FuzzySearch
    ( prefixFindCaseWithKey
    , prefixFindNoCaseWithKey
    , prefixFindNoCase
    , lookupNoCase
    , lookupNoCaseBF --redundant
    , prefixFindCaseWithKeyBF
    , prefixFindNoCaseWithKeyBF
    , noCaseKeys
    , noLowerCaseKeys
    , noCasePS
    , noLowerCasePS
    , noUmlautPS
    )
where

import           Data.Char

import           Data.StringMap.Base
import           Data.StringMap.StringSet

-- ----------------------------------------

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key.

prefixFindCaseWithKey           :: Key -> StringMap a -> [(Key, a)]
prefixFindCaseWithKey k         = toList . cutPx' (singlePS k)

prefixFindNoCaseWithKey         :: Key -> StringMap a -> [(Key, a)]
prefixFindNoCaseWithKey k       = toList . cutPx' (noCaseKeys k)

prefixFindNoCase                :: Key -> StringMap a -> [a]
prefixFindNoCase k              = elems . cutPx' (noCaseKeys k)

lookupNoCase                    :: Key -> StringMap a -> [(Key, a)]
lookupNoCase k                  = toList . cutAllPx' (noCaseKeys k)

-- ----------------------------------------

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key.
-- Breadth first variant, short words first in the result list

prefixFindCaseWithKeyBF         :: Key -> StringMap a -> [(Key, a)]
prefixFindCaseWithKeyBF k       = toListBF . cutPx' (singlePS k)

prefixFindNoCaseWithKeyBF       :: Key -> StringMap a -> [(Key, a)]
prefixFindNoCaseWithKeyBF k     = toListBF . cutPx' (noCaseKeys k)

lookupNoCaseBF                  :: Key -> StringMap a -> [(Key, a)]
lookupNoCaseBF k                = toListBF . cutAllPx' (noCaseKeys k)

-- ----------------------------------------

noCaseKeys              :: Key -> StringSet
noCaseKeys              = noCasePS . singlePS

noLowerCaseKeys         :: Key -> StringSet
noLowerCaseKeys         = noLowerCasePS . singlePS

-- ----------------------------------------


noCasePS                        :: StringSet -> StringSet
noCasePS                        = fuzzyCharPS (\ x -> [toUpper x, toLower x])

noLowerCasePS                   :: StringSet -> StringSet
noLowerCasePS                   = fuzzyCharPS (\ x -> [toUpper x, x])

-- ----------------------------------------

noUmlautPS                      :: StringSet -> StringSet
noUmlautPS                      = fuzzyCharsPS noUmlaut
    where
    noUmlaut '\196'             = ["Ae"]
    noUmlaut '\214'             = ["Oe"]
    noUmlaut '\220'             = ["Ue"]
    noUmlaut '\228'             = ["ae"]
    noUmlaut '\246'             = ["oe"]
    noUmlaut '\252'             = ["ue"]
    noUmlaut '\223'             = ["ss"]
    noUmlaut c                  = [[c]]

-- ------------------------------------------------------------
{- a few simple tests

e1 = singlePS "abc"
e2 = prefixPS "abc"
e3 = foldl unionPS emptyPS . fmap singlePS $ ["zeus","anna","anton","an"]
e4 = noCasePS e3
e5 = noLowerCasePS . singlePS $ "Data"
e6 = noUmlautPS . singlePS $ "äöüzß"

-- -}
-- ------------------------------------------------------------
