module Main where

import           Control.Applicative   ((<$>))
import qualified Data.List             as L

import           Data.StringMap.Base   (stat)
import           Data.StringMap.Strict

type Map = StringMap Int

fromListWith :: (a -> a-> a) -> [(Key, a)] -> StringMap a
fromListWith f
    = L.foldl' (\ m (k, a) -> insertWith f k a m) empty

fill :: String -> Map
fill s
    = fromListWith (+) $ zip (words s) (repeat 1)

main :: IO ()
main =
    do m <- fill <$> readFile "en_US.dict"
       let sz = size m
       let sp = space m
       let kc = keyChars m
       let ks = sum . L.map length . keys $ m
       let st = toList . stat $ m
       mapM_ putStrLn $ L.map unwords $
         [ ["size     m =", show' sz, "entries"]
         , ["space    m =", show' sp, "cells"]
         , ["keyChars m =", show' kc, "chars used for keys"]
         , ["keySize  m =", show' ks, "chars in all keys"]
         , ["stat     m =", show st]
         ]
       return ()

fmt :: Int -> String -> String
fmt n
    = reverse . take n . reverse . (replicate n ' ' ++)

show' :: (Show a) => a -> String
show' = fmt 10 . show
