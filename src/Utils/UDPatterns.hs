{-|
Module      : Utils.UDPatterns
Description : UD patterns manipulations (aka "things that might as well be 
              part of gf-ud's UDPatterns but why would anyone else need them")
Stability   : experimental
-}
module Utils.UDPatterns where

import Data.Maybe
import Data.List
import Data.List.Split
import RTree
import UDConcepts
import UDPatterns
import Utils.UDConcepts
import Utils.Misc

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
udTree2udPattern (RTree n ts) = 
  TREE (udTree2udPattern (RTree n [])) (map udTree2udPattern ts)
  -- AND [
  --   TREE (udTree2udPattern (RTree n [])) (map udTree2udPattern ts),
  --   SEQUENCE $ map udTree2udPattern ns
  -- ]
    where ns = sortBy (\n m -> compare (rootID n) (rootID m)) (RTree n []:ts)

simplifyUDPattern :: UDPattern -> UDPattern
simplifyUDPattern u = case u of
  (AND ps) -> simplifyBinOp u (map simplifyUDPattern (rmDuplicates ps))
  (OR ps) -> simplifyBinOp u (map simplifyUDPattern (rmDuplicates ps))
  (TREE p ps) -> if null ps' then p' else TREE p' ps'
    where 
      p' = simplifyUDPattern p
      ps' = map simplifyUDPattern ps 
  (TREE_ p ps) -> if null ps' then p' else TREE p' ps'
    where 
      p' = simplifyUDPattern p
      ps' = map simplifyUDPattern ps 
  -- TODO: sequence patterns
  (ARG _ _) -> simplifyUDPattern $ arg2and u
  _ -> u
  where 
    simplifyBinOp u ps = case ps of
                          [] -> TRUE
                          [p] -> p
                          ps -> u

-- | Discard UD fields from an HST pattern, excepts those explicitly listed
filterUDPattern :: [Field] -> UDPattern -> UDPattern
filterUDPattern fs p = case p of
  -- repetition could be avoided using head $ words $ show p
  (FORM _) -> if "FORM" `elem` fs then p else TRUE
  (LEMMA _) -> if "LEMMA" `elem` fs then p else TRUE
  (POS _) -> if "POS" `elem` fs then p else TRUE
  (XPOS _) -> if "XPOS" `elem` fs then p else TRUE
  (MISC _ _) -> if "MISC" `elem` fs then p else TRUE
  (FEATS s) -> if null s' then TRUE else FEATS_ s' -- _ crucial here too!
    where s' = simplifyFEATS fs s
  (FEATS_ s) -> if null s' then TRUE else FEATS_ s' 
    where s' = simplifyFEATS fs s
  (DEPREL _) -> if "DEPREL" `elem` fs then p else TRUE
  (DEPREL_ _) -> if "DEPREL_" `elem` fs then p else TRUE
  (AND ps) -> AND $ filter notTRUE (map (filterUDPattern fs) ps)
  (OR ps) -> OR $ filter notTRUE (map (filterUDPattern fs) ps)
  (NOT p) -> NOT $ filterUDPattern fs p
  (SEQUENCE ps) -> SEQUENCE $ map (filterUDPattern fs) ps
  (SEQUENCE_ ps) -> SEQUENCE_ $ map (filterUDPattern fs) ps
  (TREE p ps) -> TREE (filterUDPattern fs p) (map (filterUDPattern fs) ps)  
  (TREE_ p ps) -> TREE_ (filterUDPattern fs p) (map (filterUDPattern fs) ps)
  p@(ARG _ _) -> filterUDPattern fs (arg2and p)  
  p -> p
  where 
    notTRUE :: UDPattern -> Bool
    notTRUE TRUE = False
    notTRUE _ = True 
    simplifyFEATS :: [Field] -> String -> String
    simplifyFEATS fs s 
      | "FEATS" `elem` fs || "FEATS_" `elem` fs = s
      | (not . null) fs' = intercalate 
                    "|" 
                    (concatMap (\f -> filter (\kv -> k kv == f) ss) fs')
      | otherwise = ""
      where 
        fs' = map (drop 6) (filter (\f -> "FEATS_" `isPrefixOf` f) fs)
        k = head . splitOn "="
        ss = splitOn "|" s

-- | Shorthand for getting the morphosyntactic (POS + XPOS + FEATS + DEPREL)  
-- UD pattern corresponding to a "full" UD pattern
morphosynUDPattern :: UDPattern -> UDPattern
morphosynUDPattern = filterUDPattern morphosynFields

-- | Shorthand for getting the "universal" morphosyntactic (POS + FEATS +   
-- DEPREL) UD pattern corresponding to a "full" UD pattern
uMorphosynUDPattern :: UDPattern -> UDPattern
uMorphosynUDPattern = filterUDPattern (morphosynFields \\ ["XPOS"])

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

-- | CoNNL-U fields that appear in UD patterns
patternFields :: [Field]
patternFields = [
  "FORM", 
  "LEMMA", 
  "POS", 
  "XPOS", 
  "MISC", 
  "FEATS", 
  "FEATS_",
  "FEATS_PronType",
  "FEATS_Gender",
  "FEATS_VerbForm",
  "FEATS_NumType",
  "FEATS_Animacy",
  "FEATS_Mood",
  "FEATS_Poss",
  "FEATS_NounClass",
  "FEATS_Tense",
  "FEATS_Reflex",
  "FEATS_Number",
  "FEATS_Aspect",
  "FEATS_Foreign",
  "FEATS_Case",
  "FEATS_Voice",
  "FEATS_Abbr",
  "FEATS_Definite",
  "FEATS_Evident",
  "FEATS_Typo",
  "FEATS_Degree",
  "FEATS_Polarity",
  "FEATS_Person",
  "FEATS_Polite",
  "FEATS_Clusivity", 
  "DEPREL", 
  "DEPREL_"
  ]

-- | Check whether a certain CoNNL-U field is contained in a UD pattern
isFieldOf :: Field -> UDPattern -> Bool
isFieldOf f p = case p of 
  (FORM _) -> f == "FORM"
  (LEMMA _) -> f == "LEMMA"
  (POS _) -> f == "POS" 
  (XPOS _) -> f == "XPOS"
  (DEPREL _) -> f == "DEPREL"
  (DEPREL_ _) -> f == "DEPREL"
  (FEATS fs) -> drop 6 f `elem` keys fs
  (FEATS_ fs) -> drop 6 f `elem` keys fs
  (NOT p) -> isFieldOf f p
  (AND ps) -> any (isFieldOf f) ps
  (OR ps) -> any (isFieldOf f) ps
  (ARG _ _) -> isFieldOf f (arg2and p)
  (TREE p ps) -> isFieldOf f p || any (isFieldOf f) ps
  (TREE_ p ps) -> isFieldOf f p || any (isFieldOf f) ps
  (SEQUENCE ps) -> any (isFieldOf f) ps
  (SEQUENCE_ ps) -> any (isFieldOf f) ps
  _ -> False
  where keys fs = map (udArg . prs) (splitOn "|" fs)

parseL1L2treebank :: (FilePath,FilePath) -> IO [(UDSentence,UDSentence)]
parseL1L2treebank (p1,p2) = do
  t1 <- parseUDFile p1
  t2 <- parseUDFile p2
  return $ zip t1 t2  