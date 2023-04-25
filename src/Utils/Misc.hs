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

-- | Remove the common prefix of a zipped list
rmCommonPre :: Eq a => [(a,a)] -> [(a,a)]
rmCommonPre = dropWhile (\(p1,p2) -> p1 == p2)

-- | Remove the common postfix of a zipped list
rmCommonPost :: Eq a => [(a,a)] -> [(a,a)]
rmCommonPost = reverse . rmCommonPre . reverse

-- | Apply oth rmCommonPre and rmCommonPost to a zipped list
rmCommonPrePost :: Eq a => [(a,a)] -> [(a,a)]
rmCommonPrePost = rmCommonPost . rmCommonPre