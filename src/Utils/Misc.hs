{-|
Module      : Utils.Misc
Description : Classic utils module with all the stuff that doesn't belong
              anywhere else.
Stability   : experimental
-}
module Utils.Misc where

-- | Remove duplicates from a list, (using `elem` because it should also work
-- on UDPatterns, which do not derive Ord
rmDuplicates :: Eq a => [a] -> [a]
rmDuplicates [] = []
rmDuplicates (x:xs) | x `elem` xs = rmDuplicates xs
                    | otherwise = x:rmDuplicates xs

-- | Return all possible combinations of elements of a given list
combinations :: [a] -> [[a]]
combinations xs = sequence (replicate (length xs) xs)