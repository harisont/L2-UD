{-|
Module      : Errors
Description : Data type declarations and basic functions to manipulate L1-L2 
              errors and error patterns.
Stability   : experimental
-}

module Errors where

import Data.List
import Data.Maybe
import Data.Bifunctor
import RTree
import UDConcepts
import UDPatterns
import Utils.Misc
import Align
import Utils.UDPatterns

-- | An error (or error-correction pair) is a pair of aligned (sub)trees,  
-- pruned based on the error pattern they match
type Error = (UDTree,UDTree)

-- | Linearize the two trees errors are composed of
linearizeError :: Error -> String
linearizeError (t1,t2) = prUDTreeString t1 ++ " - " ++ prUDTreeString t2

-- | An error pattern is a pair of UD patterns
type ErrorPattern = (UDPattern,UDPattern)

-- | Shorthand to convert errors to tree error patterns
error2treePattern :: Error -> ErrorPattern
error2treePattern (e1,e2) = (udTree2treePattern e1,udTree2treePattern e2)

-- | Shorthand to convert errors to sequence error patterns
error2sequencePattern :: Error -> ErrorPattern
error2sequencePattern (e1,e2) = 
  (udTree2sequencePattern e1,udTree2sequencePattern e2)

-- | Shorthand to convert an error into a list of error patterns (as long as
-- they always are a tree and a sequence patterns, this could become a record
-- type)
error2patterns :: Error -> [ErrorPattern]
error2patterns e = [error2treePattern e, error2sequencePattern e]

-- | Shorthand to reduce error patterns to morphosyntactic error patterns
morphosynErrorPattern :: ErrorPattern -> ErrorPattern
morphosynErrorPattern (p1,p2) = (morphosynUDPattern p1,morphosynUDPattern p2)

-- | Shorthand to reduce error patterns to universal morphosyntactic error 
-- patterns
uMorphosynErrorPattern :: ErrorPattern -> ErrorPattern
uMorphosynErrorPattern (p1,p2) = 
  (uMorphosynUDPattern p1,uMorphosynUDPattern p2)

-- | Shorthand to reduce error patterns to universal syntactic error patterns
uSynErrorPattern :: ErrorPattern -> ErrorPattern
uSynErrorPattern (p1,p2) = (uSynUDPattern p1,uSynUDPattern p2)

-- | Show an error pattern as a single "L1-L2" pattern ({A -> B} syntax)
showErrorPattern :: ErrorPattern -> String
showErrorPattern (p1,p2) = "{" ++ show p1 ++ " -> " ++ show p2 ++ "}"

-- | Given an error, i.e. a pair of discrepant subtrees, prune away all pairs 
-- of aligned subtrees (hence a list of alignments is required) that are NOT 
-- discrepant, i.e. not involved in the error (used in Extract)
pruneError :: [Alignment] -> Error -> Error
pruneError as (RTree n1 t1s,RTree n2 t2s) = 
  (RTree 
    n1 
    [t1 | t1 <- t1s, 
          udTree2treePattern t1 `notElem` map udTree2treePattern t2s'],
   RTree 
    n2 
    [t2 | t2 <- t2s, 
          udTree2treePattern t2 `notElem` map udTree2treePattern t1s'])
   where (t1s',t2s') = unzip 
          [pruneError as (t1,t2) | t1 <- t1s, t2 <- t2s, (t1,t2) `elem` as]

-- | Prune an error based on the pattern it matches, i.e. prune each UDTree
-- based on the corresponding UDPattern and then discard all subtrees that are
-- not involved in any alignment (used in Match) 
pruneErrorByPattern :: ErrorPattern -> [Alignment] -> Error -> Error
pruneErrorByPattern (p1,p2) as (t1,t2) = (RTree n1 t1s', RTree n2 t2s')
  where 
    (RTree n1 t1s,RTree n2 t2s) = 
      (pruneUDTree p1 t1,pruneUDTree p2 t2)
    (t1s',t2s') = unzip [(t1',t2') | t1' <- t1s, t2' <- t2s,
                                     n1 /= n2 || t1' /= t2',
                                     (t1',t2') `elem` as]

-- | Simplify an error pattern 
simplifyErrorPattern :: ErrorPattern -> ErrorPattern
simplifyErrorPattern =  bimap simplifyUDPattern simplifyUDPattern
                        . filterFields
                        . rmIdenticalSubpatterns 
  where 
    -- filter away fields that do not vary anywhere in the L1 and L2 component
    -- and fields that only appear in one side of the pattern
    filterFields :: ErrorPattern -> ErrorPattern
    filterFields (p1,p2) = (filterUDPattern fs p1,filterUDPattern fs p2)
      where 
        fs = filter 
              (\f -> varies f p1 p2 && inboth f p1 p2)
              -- simplify by single feats 
              (patternFields \\ ["FEATS", "FEATS_"]) 
          where 
            varies f p1 p2 = filterUDPattern [f] p1 /= filterUDPattern [f] p2
            inboth f p1 p2 = f `isFieldOf` p1 && f `isFieldOf` p2
    -- simplify away pattern parts that are identical and identically placed
    -- both in the L1 and L2 pattern
    rmIdenticalSubpatterns :: ErrorPattern -> ErrorPattern
    rmIdenticalSubpatterns ep = case ep of
      (TREE p1 p1s,TREE p2 p2s) -> (TREE_ p1' p1s',TREE_ p2' p2s')
        where 
          (p1',p2') = if p1 == p2 && p1s' == p2s' 
                        then (TRUE,TRUE) 
                        else (p1,p2)
          (p1s',p2s') = filterSubpatterns p1s p2s 
      (TREE_ p1 p1s,TREE_ p2 p2s) -> (TREE_ p1 p1s',TREE_ p2 p2s')
        where
          (p1',p2') = if p1 == p2 && p1s' == p2s' 
                        then (TRUE,TRUE) 
                        else (p1,p2) 
          (p1s',p2s') = filterSubpatterns p1s p2s 
      -- simplification of sequence patterns only works if there is only one 
      -- error, like in DaLAJ sentences
      (SEQUENCE p1s,SEQUENCE p2s) -> (SEQUENCE p1s',SEQUENCE p2s')
        where (p1s',p2s') = simplifySeqs p1s p2s
      (SEQUENCE_ p1s,SEQUENCE_ p2s) -> (SEQUENCE_ p1s',SEQUENCE_ p2s')
        where (p1s',p2s') = simplifySeqs p1s p2s
      ep -> ep
      where 
        filterSubpatterns p1s p2s = if length p1s == length p2s
          then unzip $ filter (\(p1,p2) -> p1 /= p2) (p1s `zip` p2s)
          else (p1s,p2s)
        simplifySeqs p1s p2s
          -- word order error
          | length p1s == length p2s = unzip $ rmCommonPrePost $ p1s `zip` p2s
          -- missing token (assuming only one token is missing for now)
          | length p1s < length p2s = 
              let i = fromJust $ elemIndex (head (p2s \\ p1s)) p2s
              in (slice (i - 1) (i) p1s, slice (i - 1) (i + 1) p2s)
          | length p1s > length p2s = 
              let i = fromJust $ elemIndex (head (p1s \\ p2s)) p1s
              in (slice (i - 1) (i + 1) p1s, slice (i - 1) (i) p2s)
          -- redundant token (assuming only one token is redundant for now)


-- | Shorthand to convert into a Universal morphosyntactic pattern and 
-- simplify 
simplifieduMorphosynErrorPattern :: ErrorPattern -> ErrorPattern
simplifieduMorphosynErrorPattern = 
  simplifyErrorPattern . uMorphosynErrorPattern