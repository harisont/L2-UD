module Align where -- ConceptAlignment wrapper

import Data.Map (toList, empty)
import Data.Set (singleton, fromList)
import RTree
import UDConcepts
import ConceptAlignment hiding (Alignment)

type Alignment = (UDTree,UDTree)

-- ALIGNMENT CRITERIA FOR L1-L2 TREEBANKS
  
-- | Convert a comparison function into a full-blown Criterion
mkCriterion :: (UDTree -> UDTree -> Bool) -> Criterion
mkCriterion f = C f (singleton UNKNOWN) False False 

-- | List of criteria used by align, sorted by priority
criteria :: [Criterion]
criteria = [udpos, ud, pos]

{- Functions used in criteria -}

-- | Original basic criterion: matching root UD labels 
udMatch :: UDTree -> UDTree -> Bool
(RTree n ts) `udMatch` (RTree m us) = udSimpleDEPREL n == udSimpleDEPREL m

-- | POS-equivalence
posEquiv :: UDTree -> UDTree -> Bool
t1 `posEquiv` t2 = (not . null) ct1 && (ct1 == ct2)
  where (ct1, ct2) = (contentTags t1, contentTags t2)
  
ud, pos, udpos :: Criterion
ud = C udMatch (singleton UD) True False
pos = C posEquiv (singleton POS) True False 
udpos = 
  C (\t u -> udMatch t u && posEquiv t u) (fromList [UD,POS]) True True

-- | Exact same root token
sameToken :: UDTree -> UDTree -> Bool
sameToken (RTree n _) (RTree m _) = n == m

-- | Same word from
sameForm :: UDTree -> UDTree -> Bool
sameForm (RTree n _) (RTree m _) = udFORM n == udFORM m

-- | Same lemma
sameLemma :: UDTree -> UDTree -> Bool
sameLemma (RTree n _) (RTree m _) = udLEMMA n == udLEMMA m

-- | Same dependency relation
sameDeprel :: UDTree -> UDTree -> Bool
sameDeprel (RTree n _) (RTree m _) = udDEPREL n == udDEPREL m

-- | Same dependency relation (ignoring subtypes)
sameSimpleDeprel :: UDTree -> UDTree -> Bool
sameSimpleDeprel (RTree n _) (RTree m _) = 
  udSimpleDEPREL n == udSimpleDEPREL m

-- | Same root UPOS  
-- (maybe use bag of contentTags for bag of subtree POSs?)
sameUPOS :: UDTree -> UDTree -> Bool
sameUPOS (RTree n _) (RTree m _) = udUPOS n == udUPOS n

-- | alignSent wrapper to align with default "optional arguments" and return
-- pairs of alignment rather than the idiotic Alignment data type I for some
-- reason decided to use in concept-alignment
align :: (UDSentence,UDSentence) -> [Alignment]
align ss = map (\a -> (sl a,tl a)) as
  where as = toList $ alignSent empty criteria Nothing False True False ss

-- | Linearize matches, aka pairs of aligned UD (sub)sentences 
linearizeAlignment :: Alignment -> String
linearizeAlignment (s1,s2) = prUDTreeString s1 ++ " - " ++ prUDTreeString s2
  