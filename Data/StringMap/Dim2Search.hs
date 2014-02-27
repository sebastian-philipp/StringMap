{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.StringMap.Dim2Search
-- {-
    ( lookupGE
    , lookupLE
    , lookupRange
    )
-- -}
where

import           Data.StringMap.Base hiding (lookupGE, lookupLE, lookupRange)

-- ----------------------------------------

-- | remove all entries from the map with key less than the argument key

lookupGE                        :: Key -> StringMap a -> StringMap a
lookupGE                        = lookupGEX

-- start comparing first dimension

lookupGEX                       :: Key -> StringMap a -> StringMap a
lookupGEX k0                    = look k0 . norm
    where
    look [] (Val v' _t')        = (Val v' empty)
    look [] t                   = t
    look k@(c : k1) (Branch c' s' n')
        | c <  c'               = branch c' (lookupGE2 k1 s') rest
        | c == c'               = branch c' (lookupGEY k1 s') rest
        | otherwise             =                             rest
        where
          rest                  = lookupGEX k n'
    look _          Empty       = empty
    look k         (Val _v' t') = lookupGEX k t'
    look _ _                    = normError "lookupGEX"

-- continue comparing second dimension

lookupGEY                       :: Key -> StringMap a -> StringMap a
lookupGEY k0                    = look k0 . norm
    where
    look [] (Val v' _t')        = (Val v' empty)
    look [] t                   = t
    look k@(c : k1) (Branch c' s' n')
        | c <  c'               = branch c' (lookupGE2 k1 s') rest
        | c == c'               = branch c' (lookupGEX k1 s') rest
        | otherwise             =                             rest
        where
          rest                  = lookupGEY k n'
    look _          Empty       = empty
    look k         (Val _v' t') = lookupGEY k t'
    look _ _                    = normError "lookupGEY"

-- ordering of one dim is o.k., check only 2. dim (every second char)

lookupGE2                      :: Key -> StringMap a -> StringMap a
lookupGE2 k0                   = look k0 . norm
    where
    look [] (Val v' _t')        = (Val v' empty)
    look [] t                   = t
    look k@(c : k1) t@(Branch c' s' n')
        | c <  c'               = t
        | c == c'               = branch c' (lookupGE1 k1 s') n'
        | otherwise             = lookupGE2 k n'
    look _          Empty       = empty
    look k         (Val _v' t') = lookupGE2 k t'
    look _ _                    = normError "lookupGE2"

-- skip char _c and continue with comparison of next char

lookupGE1                       :: Key -> StringMap a -> StringMap a
lookupGE1 k0               = look k0 . norm
    where
    look [] (Val v' _t')        = (Val v' empty)
    look [] t                   = t
    look k@(_c : k1) (Branch c' s' n')
                                = branch c' (lookupGE2 k1 s') $ lookupGE1 k n'
    look _          Empty       = empty
    look k         (Val _v' t') = lookupGE1 k t'
    look _ _                    = normError "lookupGE1"

-- ----------------------------------------
--
-- the same stuff for less or equal

lookupLE                        :: Key -> StringMap a -> StringMap a
lookupLE                        = lookupLEX

-- start comparing first dimension

lookupLEX                       :: Key -> StringMap a -> StringMap a
lookupLEX k0                    = look k0 . norm
    where
    look [] (Val v' _t')        = (Val v' empty)
    look [] _t                  = empty
    look k@(c : k1) (Branch c' s' n')
        | c <  c'               =                             empty
        | c == c'               = branch c' (lookupLEY k1 s') empty
        | otherwise             = branch c' (lookupLE2 k1 s') (lookupLEX k n')
    look _          Empty       = empty
    look k         (Val v' t')  = val v' (lookupLEX k t')
    look _ _                    = normError "lookupLEX"

-- continue comparing second dimension

lookupLEY                       :: Key -> StringMap a -> StringMap a
lookupLEY k0                    = look k0 . norm
    where
    look [] (Val v' _t')        = (Val v' empty)
    look [] _t                  = empty
    look k@(c : k1) (Branch c' s' n')
        | c <  c'               =                             empty
        | c == c'               = branch c' (lookupLEX k1 s') empty
        | otherwise             = branch c' (lookupLE2 k1 s') (lookupLEY k n')
    look _          Empty       = empty
    look k         (Val v' t')  = val v' (lookupLEY k t')
    look _ _                    = normError "lookupLEY"

-- ordering of one dim is o.k., check only 2. dim (every second char)

lookupLE2                      :: Key -> StringMap a -> StringMap a
lookupLE2 k0                   = look k0 . norm
    where
    look [] (Val v' _t')        = (Val v' empty)
    look [] _t                  = empty
    look k@(c : k1) (Branch c' s' n')
        | c <  c'               =                             empty
        | c == c'               = branch c' (lookupLE1 k1 s') empty
        | otherwise             = branch c' s'                (lookupLE2 k n')
    look _          Empty       = empty
    look k         (Val v' t')  = val v' (lookupLE2 k t')
    look _ _                    = normError "lookupLE2"

-- skip char _c and continue with comparison of next char

lookupLE1                       :: Key -> StringMap a -> StringMap a
lookupLE1 k0                    = look k0 . norm
    where
    look [] (Val v' _t')        = (Val v' empty)
    look [] t                   = t
    look k@(_c : k1) (Branch c' s' n')
                                = branch c' (lookupLE2 k1 s') (lookupLE1 k n')
    look _          Empty       = empty
    look k         (Val v' t')  = val v' (lookupLE1 k t')
    look _ _                    = normError "lookupLE1"


-- | Combination of 'lookupLE' and 'lookupGE'
--
-- > keys $ lookupRange "a" "b" $ fromList $ zip ["", "a", "ab", "b", "ba", "c"] [1..] = ["a","ab","b"]
--
-- For all keys in @k = keys $ lookupRange lb ub m@, this property holts true: @k >= ub && k <= lb@

lookupRange                     :: Key -> Key -> StringMap a -> StringMap a
lookupRange lb ub               = lookupGE lb . lookupLE ub

-- ----------------------------------------

normError               :: String -> a
normError               = normError' "Data.StringMap.Dim2Search"

-- ----------------------------------------

