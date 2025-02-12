{-|
Module      : Align
Description : Wrapper for the ConceptAlignment library, defining 
              domain-specific criteria and offering simpler data types 
              to work with.
Stability   : experimental
-}

module Align where

import Data.List
import Data.Map (toList, empty)
import Data.Set (singleton, fromList)
import RTree
import UDStandard
import UDTrees
import ConceptAlignment hiding (Alignment)

type Alignment = (UDTree,UDTree)

-- ALIGNMENT CRITERIA FOR L1-L2 TREEBANKS
  
-- | List of criteria used by align, sorted by priority
criteria :: [Criterion]
criteria = [lemma]

{- Functions used in criteria -}
  
ud, pos, lemma, udpos, udposlemma :: Criterion
ud = C sameDeprel (singleton UD) False False
pos = C posEquiv (singleton POS) False False 
lemma = C lemmaEquiv (singleton LEMMA) False False
udpos = 
  C (\t u -> sameDeprel t u && posEquiv t u) (fromList [UD,POS]) False False
udposlemma = C 
  (\t u -> sameDeprel t u && posEquiv t u && lemmaEquiv t u) 
  (fromList [UD,POS, LEMMA]) 
  False 
  False

-- | Same word from
sameForm :: UDTree -> UDTree -> Bool
sameForm (RTree n _) (RTree m _) = udFORM n == udFORM m

-- | Same root lemma
sameLemma :: UDTree -> UDTree -> Bool
sameLemma (RTree n _) (RTree m _) = udLEMMA n == udLEMMA m

-- | Lemma-equivalence
lemmaEquiv :: UDTree -> UDTree -> Bool
t1 `lemmaEquiv` t2 = (not . null) l1s && (l1s == l2s)
  where (l1s, l2s) = (contentLemmas t1, contentLemmas t2)

-- | Same dependency relation
sameDeprel :: UDTree -> UDTree -> Bool
sameDeprel (RTree n _) (RTree m _) = 
  udDEPREL n == udDEPREL m

-- | POS-equivalence
posEquiv :: UDTree -> UDTree -> Bool
t1 `posEquiv` t2 = (not . null) ct1 && (ct1 == ct2)
  where (ct1,ct2) = (contentTags t1, contentTags t2)

-- | alignSent wrapper to align with default "optional arguments" and return
-- pairs of alignment rather than the idiotic Alignment data type I for some
-- reason decided to use in concept-alignment
align :: (UDSentence,UDSentence) -> [Alignment]
align ss = map (\a -> (sl a,tl a)) as
  where as = toList $ alignSent empty criteria Nothing False False False ss

-- | Only keep minimal alignments
minimal :: [Alignment] -> [Alignment]
minimal as = 
  filter 
    (\a@(t1,t2) -> let as' = as \\ [a] in
      not $ any (\(t1',t2') -> 
        (t1' `isSubtree` t1 && t2' `isSubtree` t2) 
        && (allNodes t1 \\ allNodes t1' == allNodes t2 \\ allNodes t2')) as' 
    )
    as