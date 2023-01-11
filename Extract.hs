module Extract where

import Data.List
import RTree
import UDConcepts
import UDPatterns
import Align
import Errors

-- | Top-level pattern extraction function used in the main.
-- The input is the list of alignments obtained for a single L1-L2 sentence,
-- the output is a list of errors
-- TODO: add pruning
extract :: [Alignment] -> [Error]
extract = smallest . morphosynErrors
  where 
    morphosynErrors = filter (not . morphosynCorrect)
    smallest as = filter (\(t1,t2) -> hasNoSuperTrees t1 t1s || hasNoSuperTrees t2 t2s) as
      where 
        hasNoSuperTrees t ts = not $ any (\t' -> isSubRTree t' t) (ts \\ [t])
        (t1s,t2s) = unzip as
    patterns = map (\(t1,t2) -> (udTree2udPattern t1, udTree2udPattern t2))

-- | Check if an alignment contains any discrepancy, i.e. an error of any kind
correct :: Alignment -> Bool 
correct (s1,s2) = prUDTreeString s1 == prUDTreeString s2

-- | Check if an alignment is morphosyntactically correct, defined as a 
-- discrepancy found upon comparing the sentences ignoring the FORM and LEMMA
-- fields.
-- Known issues: some errors that are, according to the SweLL taxonomy, 
-- classified as O-Comp, are treated as morphosyntactical too. This is the
-- case with, for instance, split compounds. 
-- NOTE on implementation: the hacky way I implemented this is to compare the
-- corresponding simplified (cf. simplifyUDPattern) UD patterns in HST 
morphosynCorrect :: Alignment -> Bool 
morphosynCorrect (s1,s2) = morphosynUDPattern s1 == morphosynUDPattern s2
