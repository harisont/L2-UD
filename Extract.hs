module Extract where

import Data.List
import RTree
import UDConcepts
import UDPatterns
import Align
import Errors
import Utils

-- | Top-level pattern extraction function used in the main.
-- The input is the list of alignments obtained for a single L1-L2 sentence,
-- the output is a list of errors
extract :: [Alignment] -> [Error]
extract = map pruned . smallest . morphosynErrors
  where 
    morphosynErrors = filter (not . morphosynCorrect)
    smallest as = 
      filter (\(t1,t2) -> noSuperTrees t1 t1s || noSuperTrees t2 t2s) as
      where 
        noSuperTrees t ts = not $ any (\t' -> isSubRTree t' t) (ts \\ [t])
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
morphosynCorrect (s1,s2) = morphosynUDPattern t1 == morphosynUDPattern t2
  where (t1,t2) = (udTree2udPattern s1,udTree2udPattern s2)

coreArgs :: [Label]
coreArgs = ["nsubj", "obj", "iobj", "csubj", "ccomp", "xcomp"]

-- | Check whether a UD subtree is a core argument, as defined in
-- https://universaldependencies.org/u/dep/index.html
isCoreArg :: UDTree -> Bool
isCoreArg (RTree n ts) = udDEPREL n `elem` coreArgs

-- | Remove non-discrepant non-core arguments from an error
-- NOTE: this is nonrecursive, as I think recursion is not needed and may 
-- even be harmful, but I am not completely sure
-- NOTE: not sure this works with all the word order errors
pruned :: Error -> Error
pruned (t1,t2) = (RTree (root t1) t1s, RTree (root t2) t2s)
  where
    subErrors = filter (not . morphosynCorrect) subAlignments
      -- NOTE: re-aligning is not efficient but passing all the alignments 
      -- around is annoying. Maybe one day with the state monad?
      where subAlignments = 
              align (udTree2adjustedSentence t1,udTree2adjustedSentence t2)
    -- NOTE: comparing patterns rather than the trees themselves to prevent 
    -- the adjustment of IDs from causing problems in the comparison implicit
    -- with the use of elem  
    t1s = filter (\t -> isCoreArg t || (udTree2udPattern t) `elem` p1s) (subtrees t1)
      where p1s = map (udTree2udPattern . fst) subErrors
    t2s = filter (\t -> isCoreArg t || (udTree2udPattern t) `elem` p2s) (subtrees t2)
      where p2s = map (udTree2udPattern . snd) subErrors