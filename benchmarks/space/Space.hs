module Main where

import Control.Applicative ((<$>))
import qualified Data.List as L

import Data.StringMap.Strict
import Data.StringMap.Base (stat)

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
       mapM_ putStrLn $
         [ "size  m = " ++ show (size m)
         , "space m = " ++ show (space m)
         , "keyChars m = " ++ show (keyChars m)
         , "stat  m = " ++ (show . toList . stat $ m)
         ]
       return ()


