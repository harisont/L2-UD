module Utils where

import UDConcepts
import UDPatterns

-- | Linearize matches, aka pairs of aligned UD (sub)sentences 
-- TODO: rn to linarizeAlignment
linearizeMatch :: (UDTree,UDTree) -> String
linearizeMatch (s1,s2) = prUDTreeString s1 ++ " - " ++ prUDTreeString s2

-- SOME HANDY TYPE SYNONYMS
type ErrorPattern = (UDPattern,UDPattern)
-- TODO: add Alignment = (UDSentence,UDSentence) or (UDTree,UDTree)