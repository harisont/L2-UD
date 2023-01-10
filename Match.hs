module Match where 

import Data.List.Split (splitOn)
import qualified Text.Regex.Posix as R
import UDConcepts (
  UDSentence,
  udSentence2tree
  )
import UDPatterns (UDPattern(..), ifMatchUDPattern)

-- | Top-level pattern matching function used in the main
match :: [(UDSentence,UDSentence)] -> [String] -> [(UDSentence,UDSentence)]
match as qs = filter (\a -> any (\p -> a `matches` p) ps) as 
  where 
    ps = map parseQuery qs

-- | Checks whether an alignment matches a particular error pattern
matches :: (UDSentence,UDSentence) -> ErrorPattern -> Bool
matches (s1,s2) (p1,p2) = ifMatchUDPattern p1 t1 && ifMatchUDPattern p2 t2
  where (t1,t2) = (udSentence2tree s1,udSentence2tree s2)

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