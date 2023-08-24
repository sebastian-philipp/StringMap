{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Main
import Data.List (foldl')
import qualified Data.StringMap.Strict as M
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

main = do
    dict <- readFile "en_US.dict"
    keys <- return $ lines dict
    elems <- return $ zip keys [1..]
    m <- return $ (M.fromList elems :: M.StringMap Int)
    defaultMainWith
        defaultConfig
      --(liftIO . evaluate $ rnf [m])
        [ bench "lookup" $ whnf (lookup keys) m
        , bench "insert" $ whnf (ins elems) M.empty
        , bench "insertWith empty" $ whnf (insWith elems) M.empty
        , bench "insertWith update" $ whnf (insWith elems) m
--        , bench "insertWith' empty" $ whnf (insWith' elems) M.empty
--        , bench "insertWith' update" $ whnf (insWith' elems) m
        , bench "insertWithKey empty" $ whnf (insWithKey elems) M.empty
        , bench "insertWithKey update" $ whnf (insWithKey elems) m
--        , bench "insertWithKey' empty" $ whnf (insWithKey' elems) M.empty
--        , bench "insertWithKey' update" $ whnf (insWithKey' elems) m
--        , bench "insertLookupWithKey empty" $ whnf (insLookupWithKey elems) M.empty
--        , bench "insertLookupWithKey update" $ whnf (insLookupWithKey elems) m
        , bench "map" $ whnf (M.map (+ 1)) m
        --, bench "mapWithKey" $ whnf (M.mapWithKey (+)) m
        , bench "foldlWithKey" $ whnf (ins elems) m
--        , bench "foldlWithKey'" $ whnf (M.foldlWithKey' sum 0) m
--        , bench "foldrWithKey" $ whnf (M.foldrWithKey consPair []) m
        , bench "delete" $ whnf (del keys) m
        , bench "update" $ whnf (upd keys) m
--        , bench "updateLookupWithKey" $ whnf (upd' keys) m
--        , bench "alter"  $ whnf (alt keys) m
--        , bench "mapMaybe" $ whnf (M.mapMaybe maybeDel) m
--        , bench "mapMaybeWithKey" $ whnf (M.mapMaybeWithKey (const maybeDel)) m
        , bench "fromList" $ whnf M.fromList elems
--        , bench "fromAscList" $ whnf M.fromAscList elems
--        , bench "fromDistinctAscList" $ whnf M.fromDistinctAscList elems
        , bench "lookupRange" $ whnf (M.lookupRange "a" "b") m
        ]
  where
    sum k v1 v2 = k + v1 + v2
    consPair k v xs = (k, v) : xs

add3 :: a -> Int -> Int -> Int
add3 _ y z = y + z
{-# INLINE add3 #-}

lookup :: [M.Key] -> M.StringMap Int -> Int
lookup xs m = foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 xs

ins :: [(M.Key, Int)] -> M.StringMap Int -> M.StringMap Int
ins xs m = foldl' (\m (k, v) -> M.insert k v m) m xs

insWith :: [(M.Key, Int)] -> M.StringMap Int -> M.StringMap Int
insWith xs m = foldl' (\m (k, v) -> M.insertWith (+) k v m) m xs

insWithKey :: [(M.Key, Int)] -> M.StringMap Int -> M.StringMap Int
insWithKey xs m = foldl' (\m (k, v) -> M.insertWithKey add3 k v m) m xs

--insWith' :: [(Int, Int)] -> M.StringMap Int -> M.StringMap Int
--insWith' xs m = foldl' (\m (k, v) -> M.insertWith' (+) k v m) m xs

--insWithKey' :: [(Int, Int)] -> M.StringMap Int -> M.StringMap Int
--insWithKey' xs m = foldl' (\m (k, v) -> M.insertWithKey' add3 k v m) m xs

--data PairS a b = PS !a !b

--insLookupWithKey :: [(Int, Int)] -> M.StringMap Int -> (Int, M.StringMap Int)
--insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
--  where
--    f (PS n m) (k, v) = let !(n', m') = M.insertLookupWithKey add3 k v m
--                        in PS (fromMaybe 0 n' + n) m'

del :: [M.Key] -> M.StringMap Int -> M.StringMap Int
del xs m = foldl' (flip M.delete) m xs

upd :: [M.Key] -> M.StringMap Int -> M.StringMap Int
upd xs m = foldl' (flip (M.update Just)) m xs

--upd' :: [Int] -> M.StringMap Int -> M.StringMap Int
--upd' xs m = foldl' (\m k -> snd $ M.updateLookupWithKey (\_ a -> Just a) k m) m xs

--alt :: [Int] -> M.StringMap Int -> M.StringMap Int
--alt xs m = foldl' (\m k -> M.alter id k m) m xs

maybeDel :: Int -> Maybe Int
maybeDel n | n `mod` 3 == 0 = Nothing
           | otherwise      = Just n
