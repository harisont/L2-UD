module Extract where

import Data.Maybe
import RTree
import UDConcepts
import UDPatterns
-- TODO: rm if hasError etc. change
import ConceptAlignment(
    Alignment, AlignedTrees(..), 
    linearize
    ) 
import Utils

-- TODO: rework & expand

-- | TODO: Top-level pattern extraction function used in the main.
-- The input is the list of alignments obtained for a single L1-L2 sentence.
extract :: [(UDSentence,UDSentence)] -> [UDPattern]
extract as = undefined
  where 
    es = filter (not . morphosynCorrect) as

-- | Check if an alignment contains any discrepancy, i.e. an error of any kind
correct :: (UDSentence,UDSentence) -> Bool 
correct (s1,s2) = linearizeSentence s1 == linearizeSentence s2

-- | Check if an alignment is morphosyntactically correct, defined as a 
-- discrepancy found upon comparing the sentences ignoring the FORM and LEMMA
-- fields.
-- Known issues: some errors that are, according to the SweLL taxonomy, 
-- classified as O-Comp, are treated as morphosyntactical too. This is the
-- case with, for instance, split compounds. 
-- NOTE on implementation: the hacky way I implemented this is to compare the
-- corresponding simplified (cf. simplifyUDPattern) UD patterns in HST 
morphosynCorrect :: (UDSentence,UDSentence) -> Bool 
morphosynCorrect (s1,s2) = pattern s1 == pattern s2
  where pattern = morphosynUDPattern . udSentence2tree

-- | Convert a UD tree into a UD pattern (HST)
-- maybe this belongs in gf-ud though
udTree2udPattern :: UDTree -> UDPattern
udTree2udPattern (RTree n []) = AND [
  FORM (udFORM n), 
  LEMMA (udLEMMA n), 
  POS (udUPOS n), 
  -- XPOS (udXPOS n), -- can be added after updating gf-ud, I think
  FEATS (prt $ udFEATS n), 
  DEPREL (udDEPREL n)
  -- no MISC cause I dunno what the second string is supposed to be:
  -- https://github.com/GrammaticalFramework/gf-ud/blob/f2705537347b417e37f1ccd156708bf066e790d6/UDPatterns.hs#L49
  ]
udTree2udPattern (RTree n ts) = 
  TREE (udTree2udPattern (RTree n [])) (map udTree2udPattern ts)

type ColumnName = String -- name of a UD column

-- | Discard certains UD columns from an HST patter
-- NOTE: only works for patterns like those generated by udTree2udPattern
simplifyUDPattern :: UDPattern -> [ColumnName] -> UDPattern
simplifyUDPattern (AND vals) cols = AND $ 
  -- there must be a better way that can be applied repeatedly but I'm tired
  catMaybes [
    if "FORM" `elem` cols then Just $ vals !! 0 else Nothing,
    if "LEMMA" `elem` cols then Just $ vals !! 1 else Nothing,
    if "POS" `elem` cols then Just $ vals !! 2 else Nothing,
    if "FEATS" `elem` cols then Just $ vals !! 3 else Nothing,
    if "DEPREL" `elem` cols then Just $ vals !! 4 else Nothing
  ]
simplifyUDPattern (TREE n ts) cols = 
  TREE (simplifyUDPattern n cols) (map (`simplifyUDPattern` cols) ts) 
simplifyUDPattern _ _ = undefined 

-- | Shorthand for getting the morphosyntactic (POS + FEATS + DEPREL) UD 
-- pattern corresponding to a given UD tree
morphosynUDPattern :: UDTree -> UDPattern
morphosynUDPattern = 
  (`simplifyUDPattern` ["POS", "FEATS", "DEPREL"]) . udTree2udPattern