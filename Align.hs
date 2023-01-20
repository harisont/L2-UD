module Align where -- ConceptAlignment wrapper

import Data.List
import Data.Map (toList, empty)
import Data.Set (singleton, fromList)
import RTree
import UDConcepts
import ConceptAlignment hiding (Alignment)

type Alignment = (UDTree,UDTree)

-- ALIGNMENT CRITERIA FOR L1-L2 TREEBANKS
  
-- | List of criteria used by align, sorted by priority
criteria :: [Criterion]
criteria = [udposlemma, udpos, lemma, ud, pos]

{- Functions used in criteria -}
  
ud, pos, lemma, udpos, udposlemma :: Criterion
ud = C sameSimpleDeprel (singleton UD) True False
pos = C posEquiv (singleton POS) True False 
lemma = C lemmaEquiv (singleton LEMMA) True False
udpos = 
  C (\t u -> sameSimpleDeprel t u && posEquiv t u) (fromList [UD,POS]) True True
udposlemma = C (\t u -> sameSimpleDeprel t u && posEquiv t u && lemmaEquiv t u) (fromList [UD,POS, LEMMA]) True True

-- | Same word from
sameForm :: UDTree -> UDTree -> Bool
sameForm (RTree n _) (RTree m _) = udFORM n == udFORM m

-- | Lemma-equivalence
lemmaEquiv :: UDTree -> UDTree -> Bool
t1 `lemmaEquiv` t2 = (not . null) l1s && (l1s == l2s)
  where (l1s, l2s) = (contentLemmas t1, contentLemmas t2)

-- | Same dependency relation (ignoring subtypes)
sameSimpleDeprel :: UDTree -> UDTree -> Bool
sameSimpleDeprel (RTree n _) (RTree m _) = 
  udSimpleDEPREL n == udSimpleDEPREL m

-- | POS-equivalence
posEquiv :: UDTree -> UDTree -> Bool
t1 `posEquiv` t2 = (not . null) ct1 && (ct1 == ct2)
  where (ct1,ct2) = (contentTags t1, contentTags t2)

-- | alignSent wrapper to align with default "optional arguments" and return
-- pairs of alignment rather than the idiotic Alignment data type I for some
-- reason decided to use in concept-alignment
align :: (UDSentence,UDSentence) -> [Alignment]
align ss = map (\a -> (sl a,tl a)) as
  where as = toList $ alignSent empty criteria Nothing False True False ss

-- | Linearize matches, aka pairs of aligned UD (sub)sentences 
linearizeAlignment :: Alignment -> String
linearizeAlignment (s1,s2) = prUDTreeString s1 ++ " - " ++ prUDTreeString s2
  
-- | Only keep minimal alignments
minimal :: [Alignment] -> [Alignment]
minimal as = 
  filter 
    (\a@(t1,t2) -> let as' = as \\ [a] in
      not $ any (\(t1',t2') -> t1' `isSubRTree` t1 && t2' `isSubRTree` t2 && isOtherwiseCorrect t1 t1' t2 t2') as' 
    )
    as
    where isOtherwiseCorrect t1 t1' t2 t2' = (udWordLines . udTree2sentence) t1 \\ (udWordLines . udTree2sentence) t1' == (udWordLines . udTree2sentence) t2 \\ (udWordLines . udTree2sentence) t2'