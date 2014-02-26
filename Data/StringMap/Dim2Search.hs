{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.StringMap.Dim2Search
{-
    ( lookupGE
    , lookupLE
    , lookupRange
    )
-}
where

import           Data.StringMap.Base hiding (lookupGE, lookupLE, lookupRange)

-- ----------------------------------------

-- | remove all entries from the map with key less than the argument key
-- 2. try

-- start comparing first dimension

lookupGEX                       :: Key -> StringMap a -> StringMap a
lookupGEX k0                    = look k0 . norm
    where
    look [] t                   = t
    look k@(c : k1) (Branch c' s' n')
        | c <  c'               = branch c' (lookupGE2 k1 s') $ lookupGEX k n'
        | c == c'               = branch c' (lookupGEY k1 s') n'
        | otherwise             = lookupGEX k n'
    look _          Empty       = empty
    look k         (Val _v' t') = lookupGEX k t'
    look _ _                    = normError "lookupGEX"

-- continue comparing second dimension

lookupGEY                       :: Key -> StringMap a -> StringMap a
lookupGEY k0                    = look k0 . norm
    where
    look [] t                   = t
    look k@(c : k1) (Branch c' s' n')
        | c <  c'               = branch c' (lookupGE2 k1 s') $ lookupGEY k n'
        | c == c'               = branch c' (lookupGEX k1 s') n'
        | otherwise             = lookupGEY k n'
    look _          Empty       = empty
    look k         (Val _v' t') = lookupGEY k t'
    look _ _                    = normError "lookupGEY"

-- ordering of one dim is o.k., check only 2. dim (every second char)

lookupGE2                      :: Key -> StringMap a -> StringMap a
lookupGE2 k0                   = look k0 . norm
    where
    look [] t                   = t
    look k@(c : k1) t@(Branch c' s' n')
        | c <  c'               = t
        | c == c'               = branch c' (lookupGE1 k1 s') n'
        | otherwise             = lookupGE2 k n'
    look _          Empty       = empty
    look k         (Val _v' t') = lookupGE2 k t'
    look _ _                    = normError "lookupGE2"

-- skip char _c and continue with comparison of next char

lookupGE1                 	:: Key -> StringMap a -> StringMap a
lookupGE1 k0               = look k0 . norm
    where
    look [] t                   = t
    look k@(_c : k1) (Branch c' s' n')
                                = branch c' (lookupGE2 k1 s') $ lookupGE1 k n'
    look _          Empty       = empty
    look k         (Val _v' t') = lookupGE1 k t'
    look _ _                    = normError "lookupGE1"



-- | remove all entries from the map with key less than the argument key
-- 1. try

lookupGE                        :: Key -> StringMap a -> StringMap a
lookupGE k0                     = look k0 . norm
    where
    look [] t                   = t
    look k@(x : k1) (Branch x' s' n')
        | x <  x'               = branch x' (lsX k1 . norm $ s') n'
        | x == x'               = branch x' (eqX k1 . norm $ s') n' -- branch x' (lookupGE k1 s') n'
        | otherwise             =            look k . norm $ n'	    -- lookupGE k n'
    look _          Empty       = empty
    look k         (Val _v' t') = look k . norm $ t'		    -- lookupGE k t'
    look _ _                    = normError "lookupGE"

    eqX [] t                    = t
    eqX k@(y : k1) (Branch y' s' n')
        | y <  y'               = branch y' (leX  k1 . norm $ s') n'
        | y == y'		= branch y' (look k1 . norm $ s') n'
        | otherwise		=            eqX  k  . norm $ n'
    eqX _          Empty        = empty
    eqX k          (Val _v' t') = eqX k . norm $ t'
    eqX _ _                     = normError "eqX"

    lsX [] t			= t
    lsX k@(y : k1) t@(Branch y' s' n')
        | y <  y'		= t
        | y == y'		= branch y' (leY k1 . norm $ s') n'
        | otherwise		=            lsX k  . norm $ n'
    lsX _          Empty	= empty
    lsX k         (Val _v' t')  = lsX k . norm $ t'
    lsX _ _			= normError "lsX"

    -- compare every second char in the sequence of key chars
    -- to be equal to or greater than the char in the search key
    -- starting with the first char
    leX [] t			= t
    leX (x : k1) t@(Branch x' s' n')
        | x <  x'		= t
        | x == x'		= branch x' (leY k1 . norm $ s') n'
        | otherwise		= empty
    leX _          Empty 	= empty
    leX k          (Val _v' t') = leX k . norm $ t'	-- or val _v' (leX ...) ???
    leX _ _                     = normError "leX"

    -- like leX, but starting with the second key char
    leY [] t			= t
    leY k@(_y : k1) (Branch y' s' n')
				= branch y' (leX k1 . norm $ s') $ (leY k . norm $ n')
    leY _          Empty        = empty
    leY k          (Val _v' t') = leY k . norm $ t'	-- or val _v' (leY ...) ???
    leY _ _			= normError "leY"

-- | remove all entries from the map with keys not having the argument key
-- as prefix and are larger than the argument key

lookupLE                        :: Key -> StringMap a -> StringMap a
lookupLE k0                     = look k0 . norm
    where
    look [] (Val v' _t')        = (Val v' empty)
    look [] _t                  = empty
    look k@(c : k1) (Branch c' s' n')
        | c <  c'               = empty
        | c == c'               = branch c' (lookupLE k1 s') empty
        | otherwise             = branch c' s' (lookupLE k n')
    look _          Empty       = empty
    look k         (Val v' t')  = val v' (lookupLE k t')
    look _ _                    = normError "lookupLE"

-- | Combination of 'lookupLE' and 'lookupGE'
-- 
-- > keys $ lookupRange "a" "b" $ fromList $ zip ["", "a", "ab", "b", "ba", "c"] [1..] = ["a","ab","b"]
-- 
-- For all keys in @k = keys $ lookupRange lb ub m@, this property holts true: @k >= ub && k <= lb@

lookupRange                     :: Key -> Key -> StringMap a -> StringMap a
lookupRange lb ub               = lookupLE ub . lookupGE lb

-- ----------------------------------------

normError               :: String -> a
normError               = normError' "Data.StringMap.Dim2Search"

-- ----------------------------------------

