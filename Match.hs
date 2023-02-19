module Match where 

import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.List.Split (splitOn)
import Data.List.Extra (replace, anySame)
import Data.String.Utils (strip)
import Data.Bifunctor
import qualified Text.Regex.Posix as R
import RTree
import UDConcepts hiding (strip)
import UDPatterns
import ConceptAlignment (udSimpleDEPREL)
import Align
import Errors
import Utils

-- | Top-level pattern matching function used in the main
match :: M.Map Field [Value] -> [String] -> [Alignment] -> [Error]
-- TODO: is minimal really necessary at this point?
match vals qs as = minimal $ concatMap (\a -> concatMap (matches a as) ps) as 
  where 
    ps = rmDuplicates $ concatMap (parseQuery vals) qs

-- | Given a pair of aligned subtrees, a list of all other alignments that
-- have been extracted for the sentece the alignment belongs to and an error 
-- patten, return any matches found for that subtree pair
matches :: Alignment -> [Alignment] -> ErrorPattern -> [Error]
matches (t1,t2) as e@(e1,e2) = 
  [pruneAlignment e as (m1,m2) | m1 <- m1s, m2 <- m2s, m1 `aligns` m2]
  where 
    (m1s,m2s) = (matchesUDPattern' e1 t1,matchesUDPattern' e2 t2)

    -- | Check, based on the previously found alignments, whether two subtrees
    -- that have been found to match a certain error pattern actually align
    -- with each other
    -- NOTE: this should probably be recursive, but for efficiency it isn't.
    -- Very deep error patterns seem not to be very likely anyway.
    aligns :: UDTree -> UDTree -> Bool
    t1@(RTree n1 t1s) `aligns` t2@(RTree n2 t2s) = 
      (t1,t2) `elem` as && case e of
        (TREE _ p1s,TREE _ p2s) -> listAligns p1s p2s t1s t2s
        (TREE_ _ p1s,TREE_ _ p2s) -> listAligns p1s p2s t1s t2s
        (SEQUENCE p1s,SEQUENCE p2s) -> 
          listAligns p1s p2s (t1:t1s) (t2:t2s)
        (SEQUENCE_ p1s,SEQUENCE_ p2s) -> 
          listAligns p1s p2s (t1:t1s) (t2:t2s)
        (_,_) -> True 
      where 
        listAligns p1s p2s t1s t2s = all 
            (\p -> any (`elem` as) [(m1,m2) | m1 <- t1s, 
                                              p `ifMatchUDPattern` m1, 
                                              m2 <- t2s, 
                                              p `ifMatchUDPattern` m2
                                   ]
            ) 
            (p1s `intersect` p2s)

-- | Custom (nonrecursive) version of GF-UD's matchesUDPattern
-- (cf. https://github.com/GrammaticalFramework/gf-ud/blob/1a4a8c1ac08c02895fa886ca20e5e7a706f484e2/UDPatterns.hs#L23-L27)
matchesUDPattern' :: UDPattern -> UDTree -> [UDTree]
matchesUDPattern' p tree@(RTree node subtrees) = case p of
  SEQUENCE ps -> maybe [] return $ findMatchingUDSequence True ps tree
  SEQUENCE_ ps -> maybe [] return $ findMatchingUDSequence False ps tree
  _ -> [tree | ifMatchUDPattern p tree]

-- | Prune an alignment that matches a certain error pattern, i.e. prune each
-- UDTree based on the corresponding UDPattern and then discard all subtrees 
-- that are not involved in any alignment 
pruneAlignment :: ErrorPattern -> [Alignment] -> Alignment -> Error
pruneAlignment (p1,p2) as (t1,t2) = (RTree n1 t1s', RTree n2 t2s')
  where 
    (RTree n1 t1s,RTree n2 t2s) = 
      (pruneUDTree p1 t1,pruneUDTree p2 t2)
    (t1s',t2s') = unzip [(t1,t2) | t1 <- t1s, t2 <- t2s,
                                   n1 /= n2 || t1 /= t2,
                                   (root t1,root t2) `elem` as']
      where as' = map (bimap root root) as

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

-- | Parses a query string into a list of error patterns, expanding variables
-- and splitting any {X->Y} shorthand. 
parseQuery :: M.Map Field [Value] -> String -> [ErrorPattern]
parseQuery vals q = 
  map (bimap read read) (expandVars (q1,q2) exps)
  where 
    -- there's a lot of read and show so both should be at hand :(
    (p1,p2) = if "->" `isInfixOf` q
                then (read (desugar q head),read (desugar q last))
                else (TRUE, read q) -- L2-only queries 
    (q1,q2) = (show p1,show p2)

    -- would be incomprehensible even with a type annotation
    -- but basically, if f == head it returns the L1 component of s, 
    --                if f == last it returns the L2 component of s
    desugar s f = case s R.=~ "\\{([^}]*)\\}" :: (String,String,String) of
      (before,"",after) -> s
      (before,match,after) -> -- head and tail remove {}
         before ++ f (splitOn "->" (tail $ init match)) ++ desugar after f

    -- variable expansions (ARGH!)
    exps = 
      concatMap (\(f,ids) -> 
        let diffCombs = 
              filter 
                (not . anySame) 
                (combinations (fromJust $ M.lookup f vals))
        in ids `zip` transpose diffCombs) vars 
      where 
        vars = map 
                (second rmDuplicates) 
                (M.toList $ M.unionWith 
                              (++) 
                              (variables M.empty p1) (variables M.empty p2)
                )

    -- | Expand variables in a sugar-free L1-L2 query (this is VERY
    -- inefficient. Variables should only be used for things that have very
    -- few possible values)
    expandVars :: (String,String) -> [(String, [Value])] -> [(String,String)]
    expandVars q [] = [q]
    expandVars q es = replaceVars es (replicate (length $ snd $ head es) q)
      where 
        replaceVars [] qs = qs
        replaceVars (e@(id,vs):es) qs = 
          replaceVars es (zipWith (\v (q1,q2) -> (replace id v q1,replace id v q2)) vs qs)

-- | Find categorical variables ("$X") in the depths of a UD pattern
variables :: M.Map Field [Value] -> UDPattern -> M.Map Field [Value]
variables m (POS s) =
  if head (strip s) == '$' then M.insertWith (++) "POS" [s] m else m
variables m (DEPREL_ s) =
  if head (strip s) == '$' then M.insertWith (++) "DEPREL_" [s] m else m
variables m (FEATS_ s) = M.unionsWith (++) (m:fms)
  where fms = map
                (\d -> let [k,v] = splitOn "=" d in 
                    if head (strip v) == '$' 
                      then M.singleton ("FEATS_" ++ k) [v] 
                      else M.empty
                ) 
                (splitOn "|" s)
variables m (AND ps) = M.unionsWith (++) (map (variables m) ps)
variables m (OR ps) = M.unionsWith (++) (map (variables m) ps)
variables m (ARG pos deprel) = variables m (AND [POS pos, DEPREL deprel])
variables m (SEQUENCE ps) = M.unionsWith (++) (map (variables m) ps)
variables m (SEQUENCE_ ps) = M.unionsWith (++) (map (variables m) ps)
variables m (NOT p) = variables m p
variables m (TREE p ps) = 
  M.unionsWith (++) (variables m p:map (variables m) ps)
variables m (TREE_ p ps) = 
  M.unionsWith (++) (variables m p:map (variables m) ps)
variables m _ = m
        