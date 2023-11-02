{-|
Module      : Utils.UD
Description : Boring UD-specific stuff, including some shorthands for things
              implemented in gf-ud's UDConcepts module.
Stability   : experimental
-}

module Utils.UDConcepts where

import Data.List
import qualified Data.Map as M
import RTree
import UDConcepts

-- | Return the words a UD sentence is composed of, ignoring unsplit tokens.
-- For instance such as "dello", usually tokenized as di + lo
-- (cf. https://github.com/harisont/L2-UD/issues/12)
udWords :: UDSentence -> [UDWord]
udWords s = filter ((not . isRange) . udID) (udWordLines s)
  where
    isRange :: UDId -> Bool
    isRange (UDIdRange _ _) = True
    isRange _ = False

-- | Create root token, convert to sentence and adjust the IDs
udTree2adjustedSentence :: UDTree -> UDSentence
udTree2adjustedSentence = adjustUDIds . udTree2sentence . createRoot

-- | Return the ID of the root of a UD (sub)tree
rootID :: UDTree -> UDId
rootID (RTree n _) = udID n

-- | Merge a list of UDTrees with the same structure
-- TODO: I dunno if this is really needed?
mergeUDTrees :: [UDTree] -> UDTree
mergeUDTrees [] = error "Trying to merge an empty list of UD trees!"
mergeUDTrees [t] = t
mergeUDTrees (t1:t2:ts) = mergeUDTrees ((t1 `merge` t2):ts)
    where 
      t1@(RTree n1 t1s) `merge` t2@(RTree n2 t2s) = 
        if n1 == n2 
          then RTree n1 (map 
                mergeUDTrees 
                (groupBy (\t1 t2 -> root t1 == root t2) (t1s ++ t2s))
                )
          else error "Trying to merge UD trees with nonmatching roots!"


-- | Names of CoNNL-U "columns" or morphological feature 
-- (morpho features are called e.g. FEATS_Gender)
type Field = String 

-- | Values of the various "fields" of a CoNLL-U file
type Value = String 

--  | Morphosyntax-relevant columns
morphosynFields :: [Field]
morphosynFields = ["POS", "XPOS", "FEATS", "FEATS_", "DEPREL"]

-- | Check whether a UD subtree is a core argument, as defined in
-- https://universaldependencies.org/u/dep/index.html
isCoreArg :: UDTree -> Bool
isCoreArg (RTree n _) = udDEPREL n `elem` coreArgs
  where coreArgs = ["nsubj", "obj", "iobj", "csubj", "ccomp", "xcomp"]

fieldVals :: M.Map Field [Value]
fieldVals = M.fromList [
  ("POS", ["ADJ", "ADP", "PUNCT", "ADV", "AUX", "SYM", "INTJ", "CCONJ", "X", "NOUN", "DET", "PROPN", "NUM", "VERB", "PART", "PRON", "SCONJ"]),
  ("DEPREL_", ["nsubj", "obj", "iobj", "csubj", "ccomp", "xcomp", "obl", "vocative", "expl", "dislocated", "advcl", "advmod", "discourse", "aux", "cop", "mark", "nmod", "appos", "nummod", "acl", "amod", "det", "clf", "case", "conj", "cc", "fixed", "flat", "compound", "list", "parataxis", "orphan", "goeswith", "reparandum", "punct", "root", "dep"]),
  ("FEATS_PronType", ["Art", "Dem", "Emp", "Exc", "Ind", "Int", "Neg", "Prs", "Rcp", "Rel", "Tot"]),
  ("FEATS_Gender", ["Com", "Fem", "Masc", "Neut"]),
  ("FEATS_VerbForm", ["Conv", "Fin", "Gdv", "Ger", "Inf", "Part", "Sup", "Vnoun"]),
  ("FEATS_NumType", ["Card", "Dist", "Frac", "Mult", "Ord", "Range", "Sets"]),
  ("FEATS_Animacy", ["Anim", "Hum", "Inan", "Nhum"]),
  ("FEATS_Mood", ["Adm", "Cnd", "Des", "Imp", "Ind", "Int", "Irr", "Jus", "Nec", "Opt", "Pot", "Prp", "Qot", "Sub"]),
  ("FEATS_Poss", ["Yes"]),
  ("FEATS_NounClass", ["Bantu1", "Bantu2", "Bantu3", "Bantu4", "Bantu5", "Bantu6", "Bantu7", "Bantu8", "Bantu9", "Bantu10, Bantu11", "Bantu12", "Bantu13", "Bantu14", "Bantu15", "Bantu16", "Bantu17", "Bantu18", "Bantu19", "Bantu20", "Bantu21", "Bantu22", "Bantu23", "Wol1", "Wol2", "Wol3", "Wol4", "Wol5", "Wol6", "Wol7", "Wol8", "Wol9", "Wol10", "Wol11", "Wol12"]),
  ("FEATS_Tense", ["Fut", "Imp", "Past", "Pqp", "Pres"]),
  ("FEATS_Reflex", ["Yes"]),
  ("FEATS_Number", ["Coll", "Count", "Dual", "Grpa", "Grpl", "Inv", "Pauc", "Plur", "Ptan", "Sing", "Tri"]),
  ("FEATS_Aspect", ["Hab", "Imp", "Iter", "Perf", "Prog", "Prosp"]),
  ("FEATS_Foreign", ["Yes"]),
  ("FEATS_Case", ["Abs", "Acc", "Erg", "Nom", "Abe", "Ben", "Cau", "Cmp", "Cns", "Com", "Dat", "Dis", "Equ", "Gen", "Ins", "Par", "Tem", "Tra", "Voc", "Abl", "Add", "Ade", "All", "Del", "Ela", "Ess", "Ill", "Ine", "Lat", "Loc", "Per", "Sbe", "Sbl", "Spl", "Sub", "Sup", "Ter"]),
  ("FEATS_Voice", ["Act", "Antip", "Bfoc", "Cau", "Dir", "Inv", "Lfoc", "Mid", "Pass", "Rcp"]),
  ("FEATS_Abbr", ["Yes"]),
  ("FEATS_Definite", ["Com", "Cons", "Def", "Ind", "Spec"]),
  ("FEATS_Evident", ["Fh", "Nfh"]),
  ("FEATS_Typo", ["Yes"]),
  ("FEATS_Degree", ["Abs", "Aug", "Cmp", "Dim", "Equ", "Pos", "Sup"]),
  ("FEATS_Polarity", ["Neg", "Pos"]),
  ("FEATS_Person", ["0", "1", "2", "3", "4"]),
  ("FEATS_Polite", ["Elev", "Form", "Humb", "Infm"]),
  ("FEATS_Clusivity", ["Ex", "In"])
  ]

dummyUDTree :: UDTree
dummyUDTree = RTree (initUDWord 0) []