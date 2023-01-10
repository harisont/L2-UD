module Align where -- ConceptAlignment wrapper

import Data.Map (toList, empty)
import Data.Set (singleton)
import RTree
import UDConcepts
import ConceptAlignment

-- ALIGNMENT CRITERIA FOR L1-L2 TREEBANKS
  
-- | Convert a comparison function into a full-blown Criterion
mkCriterion :: (UDTree -> UDTree -> Bool) -> Criterion
mkCriterion f = C f (singleton UNKNOWN) False False 

-- | Ordered list of criteria
criteria :: [Criterion]
criteria = map mkCriterion [
  sameToken,
  sameForm,
  sameLemma,
  sameDeprel
  ]

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
align :: [UDSentence] -> [UDSentence] -> [(UDSentence,UDSentence)]
align ss1 ss2 = 
  map alignment2sentencePair as
  where as = concatMap 
          (toList . alignSent empty criteria Nothing False True False) 
          (zip ss1 ss2)
  