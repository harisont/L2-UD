module Utils where

import Data.List
import RTree
import UDConcepts

udTree2adjustedSentence :: UDTree -> UDSentence
udTree2adjustedSentence = adjustUDIds . udTree2sentence . createRoot

rootID :: UDTree -> UDId
rootID (RTree n _) = udID n

-- using `elem` because UDPatterns do not derive Ord
rmDuplicates :: Eq a => [a] -> [a]
rmDuplicates [] = []
rmDuplicates (x:xs) | x `elem` xs = rmDuplicates xs
                    | otherwise = x:rmDuplicates xs

combinations :: [a] -> [[a]]
combinations xs = sequence (replicate (length xs) xs)