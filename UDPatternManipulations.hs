{-|
Module      : UDPatterns
Description : UD patterns manipulations (aka "things that might as well be 
              part of gf-ud's UDPatterns but why would anyone else need them")
Stability   : experimental
-}
module UDPatternManipulations where

import Data.Maybe
import Data.List
import RTree
import UDConcepts
import UDPatterns
import UD

-- | Convert a UD tree into a UD pattern
udTree2udPattern :: UDTree -> UDPattern
udTree2udPattern (RTree n []) = AND [
  FORM (udFORM n), 
  LEMMA (udLEMMA n), 
  POS (udUPOS n), 
  XPOS (udXPOS n),
  FEATS (prt $ udFEATS n), 
  DEPREL (udDEPREL n)
  -- no MISC cause I dunno what the second string is supposed to be:
  -- https://github.com/GrammaticalFramework/gf-ud/blob/f2705537347b417e37f1ccd156708bf066e790d6/UDPatterns.hs#L49
  ]
udTree2udPattern (RTree n ts) = AND [
  TREE (udTree2udPattern (RTree n [])) (map udTree2udPattern ts),
  SEQUENCE $ map udTree2udPattern ns
  ]
    where ns = sortBy (\n m -> compare (rootID n) (rootID m)) (RTree n []:ts)

-- | Discard UD columns from an HST pattern, excepts those explicitly listed
-- NOTE: only works for patterns like those generated by udTree2udPattern
simplifyUDPattern :: UDPattern -> [Field] -> UDPattern
simplifyUDPattern (AND [TREE n ts, SEQUENCE ns]) cols = AND [
  simplifyUDPattern (TREE n ts) cols, 
  simplifyUDPattern (SEQUENCE ns) cols]
simplifyUDPattern (AND vals) cols = 
  AND $ mapMaybe (\col -> find (\val -> col `isPrefixOf` show val) vals) cols
simplifyUDPattern (TREE n ts) cols = 
  TREE (simplifyUDPattern n cols) (map (`simplifyUDPattern` cols) ts)
simplifyUDPattern (SEQUENCE ns) cols = 
  SEQUENCE (map (`simplifyUDPattern` cols) ns) 
simplifyUDPattern p _ = p 

-- | Shorthand for getting the morphosyntactic (POS + XPOS + FEATS + DEPREL)  
-- UD pattern corresponding to a "full" UD pattern
morphosynUDPattern :: UDPattern -> UDPattern
morphosynUDPattern = flip simplifyUDPattern morphosynColumns

-- | Shorthand for getting the "universal" morphosyntactic (POS + FEATS +   
-- DEPREL) UD pattern corresponding to a "full" UD pattern
uniMorphosynUDPattern :: UDPattern -> UDPattern
uniMorphosynUDPattern = flip simplifyUDPattern (morphosynColumns \\ ["XPOS"])

-- | Remove the parts of a tree not described by a certain UDPattern 
pruneUDTree :: UDPattern -> UDTree -> UDTree
pruneUDTree p t = case p of
  (FORM _) -> pruneSingleTokenPattern t p
  (LEMMA _) -> pruneSingleTokenPattern t p 
  (POS _) -> pruneSingleTokenPattern t p
  (DEPREL _) -> pruneSingleTokenPattern t p
  (DEPREL_ _) -> pruneSingleTokenPattern t p 
  (FEATS _) -> pruneSingleTokenPattern t p
  (FEATS_ _) -> pruneSingleTokenPattern t p
  (NOT _) -> t -- NOTE: return t rn, cause idk how to define pruning for NOTs
  -- is it OK that AND and OR behave identically?
  (AND ps) -> mergeUDTrees $ map (`pruneUDTree` t) ps
  (OR ps) -> mergeUDTrees $ map (`pruneUDTree` t) ps
  (ARG _ _) -> pruneUDTree (arg2and p) t
  (TREE p ps) -> pruneSubtrees (filterSubtrees t p ps) ps
  (TREE_ p ps) -> pruneSubtrees (filterSubtrees t p ps) ps
  -- pruning is hacky and not optimal for sequence patterns, but the 
  -- alternative is complicated (returning lists of UDTrees) and 
  -- computationally expensive 
  (SEQUENCE ps) -> pruneSubtrees (filterSubtrees t TRUE ps) ps
  (SEQUENCE_ ps) -> pruneSubtrees (filterSubtrees t TRUE ps) ps
  _ -> t
  where
    pruneSingleTokenPattern :: UDTree -> UDPattern -> UDTree
    pruneSingleTokenPattern t p = 
      fst $ replacementsWithUDPattern (PRUNE p 0) t
    filterSubtrees :: UDTree -> UDPattern -> [UDPattern] -> UDTree
    filterSubtrees t p ps = 
      fst $ replacementsWithUDPattern (FILTER_SUBTREES p (OR ps)) t
    pruneSubtrees :: UDTree -> [UDPattern] -> UDTree
    pruneSubtrees t [] = t
    pruneSubtrees (RTree n ts) (p:ps) = 
      pruneSubtrees (RTree n (map (pruneUDTree p) ts)) ps

-- | Desugar ARG patterns
arg2and :: UDPattern -> UDPattern
arg2and (ARG p d) = AND [POS p, DEPREL d]
arg2and _ = error "Attempt to desugar non-ARG pattern!"