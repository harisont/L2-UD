module Match where 

import Data.List.Split (splitOn)
import qualified Text.Regex.Posix as R
import UDConcepts (UDSentence)
import UDPatterns (UDPattern(..), ifMatchUDPattern)
import ConceptAlignment (Alignment, sl, tl)

-- | Top-level pattern matching function used in the main
patternMatch :: [Alignment] -> [String] -> [UDSentence]
patternMatch alignments queries = concatMap (filter matches alignments) patterns
  where 
    patterns = map parseQuery queries

-- | Checks whether an alignment matches a particular error pattern
matches :: Alignment -> ErrorPattern -> Bool
matches a (l1,l2) = ifMatchUDPattern l1 (sl a) && ifMatchUDPattern l2 (tl a)

type ErrorPattern = (UDPattern, UDPattern)

-- | Parses a query string into an error pattern, simplifying any {X->Y}
-- shorthand 
parseQuery :: String -> ErrorPattern
parseQuery q = (read (replace q head), read (replace q last))
  where 
    replace s f = case  s R.=~ "\\{([^}]*)\\}" :: (String,String,String) of
      (before,"",after) -> s
      -- head and tail remove {}
      (before,match,after) ->
         before ++ f (splitOn "->" (tail $ init match)) ++ replace after f