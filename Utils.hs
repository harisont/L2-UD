{-|
Module      : UD
Description : Classic module with all the stuff that doesn't belong anywhere
              else.
Stability   : experimental
-}
module Utils where

import Data.Maybe
import Data.List
import RTree
import UDConcepts
import UDPatterns
import Align

-- | Only keep minimal alignments
-- TODO: is there a better place for this?
minimal :: [Alignment] -> [Alignment]
minimal as = 
  filter 
    (\a@(t1,t2) -> let as' = as \\ [a] in
      not $ any (\(t1',t2') -> t1' `isSubRTree` t1 && t2' `isSubRTree` t2) as' 
    )
    as

-- | Remove duplicates from a list, (using `elem` because it should also work
-- on UDPatterns, which do not derive Ord
rmDuplicates :: Eq a => [a] -> [a]
rmDuplicates [] = []
rmDuplicates (x:xs) | x `elem` xs = rmDuplicates xs
                    | otherwise = x:rmDuplicates xs

-- | Return all possible combinations of elements of a given list
combinations :: [a] -> [[a]]
combinations xs = sequence (replicate (length xs) xs)