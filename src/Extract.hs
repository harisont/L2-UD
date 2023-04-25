{-|
Module      : Extract
Description : Functions for error pattern extraction from L1-L2 treebanks.
Stability   : experimental
-}

module Extract where

import UDConcepts
import Align
import Errors
import Utils.Misc
import Utils.UDPatterns

-- | Top-level pattern extraction function used in the main.
-- The input is the list of alignments obtained for a single L1-L2 sentence,
-- the output is a list of errors
extract :: [Alignment] -> [Error]
extract = filter (not . morphosynCorrect)

-- | Check if an alignment contains any discrepancy, i.e. an error of any kind
-- (aka check if an alignment is in fact an Error)
correct :: Alignment -> Bool 
correct (s1,s2) = prUDTreeString s1 == prUDTreeString s2

-- | Check if an alignment is morphosyntactically correct, defined as a 
-- discrepancy found upon comparing the sentences ignoring the FORM and LEMMA
-- fields.
-- Known issues: some errors that are, according to the SweLL taxonomy, 
-- classified as O-Comp, are treated as morphosyntactical too. This is the
-- case with, for instance, split compounds. 
-- NOTE on implementation: the hacky way I implemented this is to compare the
-- corresponding simplified (cf. filterUDPattern) UD patterns in HST 
-- TODO: create and use U-version
morphosynCorrect :: Alignment -> Bool 
morphosynCorrect (s1,s2) = 
  all ((\(p1,p2) -> p1 == p2) . morphosynErrorPattern) ps
  where ps = [(udTree2treePattern s1,udTree2treePattern s2),
              (udTree2sequencePattern s1,udTree2sequencePattern s2)]