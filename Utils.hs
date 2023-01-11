module Utils where

import UDConcepts
import UDPatterns

-- | Turns a CoNNL-U sentence into linearized plain text
linearizeSentence :: UDSentence -> String
linearizeSentence = prUDTreeString . udSentence2tree

-- | Linearize matches, aka pairs of aligned UD (sub)sentences 
linearizeMatch :: (UDSentence,UDSentence) -> String
linearizeMatch (s1,s2) = linearizeSentence s1 ++ " - " ++ linearizeSentence s2

-- SOME HANDY TYPE SYNONYMS
type ErrorPattern = (UDPattern,UDPattern)
-- TODO: add Alignment = (UDSentence,UDSentence) or (UDTree,UDTree)