{-# OPTIONS -XBangPatterns #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.StringSet
  Copyright  : Copyright (C) 2010 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: not portable

  A simplified version of StringMap for implementing sets.

  There is one important difference to the StringMap implementation:
  The fields are not marked to be strict. This enables building the
  set on the fly.

  This feature is used in fuzzy search, when an index tree is restricted
  to a set of keys, e.g. the set of all none case significant keys

-}

-- ----------------------------------------------------------------------------

module Data.StringMap.StringSet
where

import           Data.List            (nub, sort)

import           Data.StringMap.Types

-- ----------------------------------------

-- | Set of strings implemented as lazy prefix tree.
-- @type StringSet = StringMap ()@ is not feasable because of
-- the strict fields in the StringMap definition

data StringSet                  = PSempty
                                | PSelem  StringSet
                                | PSnext  Sym StringSet StringSet
                                  deriving (Show)

emptyPS                         :: StringSet
emptyPS                         = PSempty

elemPS                          :: StringSet -> StringSet
elemPS s@(PSelem _)             = s
elemPS s                        = PSelem s

elem0PS                         :: StringSet
elem0PS                         = elemPS emptyPS

nextPS                          :: Sym -> StringSet -> StringSet -> StringSet
nextPS _ PSempty n              = n
nextPS s c       n              = PSnext s c n

lastPS                          :: Sym -> StringSet -> StringSet
lastPS s c                      = nextPS s c emptyPS

nullPS                          :: StringSet -> Bool
nullPS PSempty                  = True
nullPS _                        = False

singlePS                        :: Key -> StringSet
singlePS                        = foldr (\ c r -> lastPS c r)          elem0PS

-- ------------------------------------------------------------

prefixPS                        :: Key -> StringSet
prefixPS                        = foldr (\ c r -> elemPS (lastPS c r)) elem0PS

-- ------------------------------------------------------------

unionPS                         :: StringSet -> StringSet -> StringSet
unionPS PSempty ps2             = ps2
unionPS ps1     PSempty         = ps1

unionPS (PSelem ps1) (PSelem ps2)       = PSelem (unionPS ps1 ps2)
unionPS (PSelem ps1)         ps2        = PSelem (unionPS ps1 ps2)
unionPS         ps1  (PSelem ps2)       = PSelem (unionPS ps1 ps2)

unionPS ps1@(PSnext c1 s1 n1)
        ps2@(PSnext c2 s2 n2)
    | c1 < c2                   = nextPS c1 s1 (unionPS n1 ps2)
    | c1 > c2                   = nextPS c2 s2 (unionPS ps1 n2)
    | otherwise                 = nextPS c1 (unionPS s1 s2) (unionPS n1 n2)

-- ------------------------------------------------------------

foldPS                          :: (Key -> b -> b) -> b -> (Key -> Key) -> StringSet -> b
foldPS _ r _   PSempty          = r
foldPS f r kf (PSelem ps1)      = let r' = foldPS f r kf ps1
                                  in
                                  f (kf []) r'
foldPS f r kf (PSnext c1 s1 n1) = let r' = foldPS f r kf n1
                                  in
                                  foldPS f r' (kf . (c1:)) s1

foldWithKeyPS                   :: (Key -> b -> b) -> b -> StringSet -> b
foldWithKeyPS f e               = foldPS f e id

-- ------------------------------------------------------------

elemsPS                         :: StringSet -> [Key]
elemsPS                         = foldWithKeyPS (:) []

-- ------------------------------------------------------------

fuzzyCharPS                     :: (Sym -> [Sym]) -> StringSet -> StringSet
fuzzyCharPS _  PSempty          = PSempty
fuzzyCharPS f (PSelem ps)       = PSelem $ fuzzyCharPS f ps
fuzzyCharPS f (PSnext c s n)    = unionPS ps1 (fuzzyCharPS f n)
    where
    s'                          = fuzzyCharPS f s
    cs                          = sort . nub . f $ c
    ps1                         = foldr (\ c' r' -> nextPS c' s' r') emptyPS $ cs

-- ------------------------------------------------------------

fuzzyCharsPS                    :: (Sym -> [Key]) -> StringSet -> StringSet
fuzzyCharsPS _  PSempty         = PSempty
fuzzyCharsPS f (PSelem ps)      = PSelem $ fuzzyCharsPS f ps
fuzzyCharsPS f (PSnext c s n)   = unionPS ps1 (fuzzyCharsPS f n)
    where
    s'                          = fuzzyCharsPS f s
    cs                          = sort . nub . f $ c
    ps1                         = foldr (\ w' r' -> nextPSw w' s' r') emptyPS $ cs
    nextPSw [] _ r'             = r'
    nextPSw (x:xs) s'' r'       = nextPS x (foldr lastPS s'' xs) r'

-- ------------------------------------------------------------
