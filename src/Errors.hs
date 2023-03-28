{-|
Module      : Errors
Description : Data type declarations and basic functions to manipulate L1-L2 
              errors and error patterns.
Stability   : experimental
-}

module Errors where

import Data.List
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

-- | Shorthand to convert errors to error patterns
error2Pattern :: Error -> ErrorPattern
error2Pattern (e1,e2) = (udTree2udPattern e1,udTree2udPattern e2)

-- | Shorthand to convert errors to morphosyntactic error patterns
error2morphosynPattern :: Error -> ErrorPattern
error2morphosynPattern e = (morphosynUDPattern p1,morphosynUDPattern p2)
  where (p1,p2) = error2Pattern e

-- | Shorthand to convert errors to universal morphosyntactic error patterns
error2uniMorphosynPattern :: Error -> ErrorPattern
error2uniMorphosynPattern e = 
  (uniMorphosynUDPattern p1,uniMorphosynUDPattern p2)
  where (p1,p2) = error2Pattern e

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
    [t1 | t1 <- t1s, udTree2udPattern t1 `notElem` map udTree2udPattern t2s'],
   RTree 
    n2 
    [t2 | t2 <- t2s, udTree2udPattern t2 `notElem` map udTree2udPattern t1s'])
   where (t1s',t2s') = unzip 
          [pruneError as (t1,t2) | t1 <- t1s, t2 <- t2s, (t1,t2) `elem` as]

-- | Prune an error based on the pattern it matches, i.e. prune each UDTree
-- based on the corresponding UDPattern and then discard all subtrees that are
-- not involved in any alignment (used in Match) 
pruneErrorByPattern :: ErrorPattern -> [Alignment] -> Alignment -> Error
pruneErrorByPattern (p1,p2) as (t1,t2) = (RTree n1 t1s', RTree n2 t2s')
  where 
    (RTree n1 t1s,RTree n2 t2s) = 
      (pruneUDTree p1 t1,pruneUDTree p2 t2)
    (t1s',t2s') = unzip [(t1',t2') | t1' <- t1s, t2' <- t2s,
                                   n1 /= n2 || t1' /= t2',
                                   (t1',t2') `elem` as]

-- | Simplify an error pattern removing fields that do not present any 
-- variation between the L1 and L2 component
simplifyErrorPattern :: ErrorPattern -> ErrorPattern
simplifyErrorPattern (p1,p2) = 
  (simplifyUDPattern fs p1,simplifyUDPattern fs p2)
  where fs = filter 
              (\f -> simplifyUDPattern [f] p1 /= simplifyUDPattern [f] p2)
              -- simplify by single feats 
              (patternFields \\ ["FEATS", "FEATS_"]) 