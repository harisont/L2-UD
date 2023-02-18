module Match where 

import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.List.Split (splitOn)
import Data.List.Extra (replace, anySame)
import Data.String.Utils (strip)
import qualified Text.Regex.Posix as R
import RTree
import UDConcepts hiding (strip)
import UDPatterns
import ConceptAlignment (udSimpleDEPREL)
import Align
import Errors
import Utils

-- | Top-level pattern matching function used in the main
match :: M.Map Field [Value] -> [String] -> [Alignment] -> [Alignment]
-- TODO: is minimal really necessary at this point?
match vals qs as = minimal $ concatMap (\a -> concatMap (matches a) ps) as 
  where 
    ps = rmDuplicates $ concatMap (parseQuery vals) qs 
    matches :: Alignment -> ErrorPattern -> [Alignment]
    matches (t1,t2) e@(e1,e2) = [(m1,m2) | m1 <- m1s, 
                                           m2 <- m2s, 
                                           aligns m1 m2 e]
      where 
        (m1s,m2s) = (matchesUDPattern' e1 t1,matchesUDPattern' e2 t2)

        aligns :: UDTree -> UDTree -> ErrorPattern -> Bool
        aligns t1@(RTree n1 t1s) t2@(RTree n2 t2s) e = 
          (t1,t2) `elem` as && case e of
            (NOT p1,NOT p2) -> aligns t1 t2 (p1,p2) -- ?
            (AND p1s,AND p2s) -> all (aligns' t1 t2) (common p1s p2s) -- ?
            (OR p1s,OR p2s) -> all (aligns' t1 t2) (common p1s p2s) -- ?
            (a1@(ARG _ _),a2@(ARG _ _)) -> 
              aligns t1 t2 (arg2and a1,arg2and a2)
            (SEQUENCE p1s,SEQUENCE p2s) -> undefined
            (SEQUENCE_ p1s,SEQUENCE_ p2s) -> undefined
            (TREE _ p1s,TREE _ p2s) -> all (\(m1,m2) -> and [if isJust m1 && isJust m2 && ifMatchUDPattern (fromJust m1) t1 && ifMatchUDPattern (fromJust m2) t2 then aligns' t1 t2 (m1,m2) else True | t1 <- t1s, t2 <- t2s]) (common p1s p2s) 
            (TREE_ _ p1s,TREE_ _ p2s) -> all (\(m1,m2) -> and [if isJust m1 && isJust m2 && ifMatchUDPattern (fromJust m1) t1 && ifMatchUDPattern (fromJust m2) t2 then aligns' t1 t2 (m1,m2) else True | t1 <- t1s, t2 <- t2s]) (common p1s p2s) 
            (_,_) -> True -- single-token pattern and nonmatching patterns (?)
        
        aligns' :: UDTree -> UDTree -> (Maybe UDPattern,Maybe UDPattern) -> Bool
        aligns' t1 t2 (m1,m2) = (isNothing m1 || isNothing m2) 
                                || aligns t1 t2 (fromJust m1,fromJust m2)

        common :: [UDPattern] -> [UDPattern] -> [(Maybe UDPattern,Maybe UDPattern)]
        common p1s p2s = 
          [(Just p1, find (sameConstructor p1) p2s) | p1 <- p1s] 

-- | Remove the parts of a tree not involved in a certain pattern 
-- (for sequence patterns, this can result in a forest, hence the return type)
pruneUDTree :: UDPattern -> UDTree -> [UDTree]
pruneUDTree p t = case p of
  (FORM _) -> [pruneSingleTokenPattern t p]
  (LEMMA _) -> [pruneSingleTokenPattern t p] 
  (POS _) -> [pruneSingleTokenPattern t p]
  (DEPREL _) -> [pruneSingleTokenPattern t p]
  (DEPREL_ _) -> [pruneSingleTokenPattern t p] 
  (FEATS _) -> [pruneSingleTokenPattern t p]
  (FEATS_ _) -> [pruneSingleTokenPattern t p]
  (NOT _) -> [t] -- return t rn, cause idk how to define pruning for NOTs :/
  -- is it OK that AND and OR behave identically?
  (AND ps) -> [mergeUDTrees $ concatMap (`pruneUDTree` t) ps]
  (OR ps) -> [mergeUDTrees $ concatMap (`pruneUDTree` t) ps]
  (ARG _ _) -> pruneUDTree (arg2and p) t
  (SEQUENCE ps) -> adjacent (UDIdInt $ length ps) (filterPruneSequence t ps) 
    where 
      adjacent (UDIdInt n) (t:ts) = let i = udid2int $ udID $ root t in
        t:filter 
            (\t' -> udid2int (udID $ root t') `elem` [i..i + (n - 1)]) 
            ts 
  (SEQUENCE_ ps) -> filterPruneSequence t ps
  -- is it OK that TREE and TREE_ behave identically?
  (TREE p ps) -> [pruneSubtrees (filterSubtrees t p ps) ps]
  (TREE_ p ps) -> [pruneSubtrees (filterSubtrees t p ps) ps]
  _ -> [t]
  where
    pruneSingleTokenPattern :: UDTree -> UDPattern -> UDTree
    pruneSingleTokenPattern t p = 
      fst $ replacementsWithUDPattern (PRUNE p 0) t
    filterPruneSequence :: UDTree -> [UDPattern] -> [UDTree]
    filterPruneSequence t = concatMap (\p -> concatMap (pruneUDTree p) (matchesUDPattern p t))
    filterSubtrees :: UDTree -> UDPattern -> [UDPattern] -> UDTree
    filterSubtrees t p ps = 
      fst $ replacementsWithUDPattern (FILTER_SUBTREES p (OR ps)) t
    pruneSubtrees :: UDTree -> [UDPattern] -> UDTree
    pruneSubtrees t [] = t
    pruneSubtrees (RTree n ts) (p:ps) = 
      pruneSubtrees (RTree n (concatMap (pruneUDTree p) ts)) ps
    
--pruneAlignment (RTree n1 t1s,RTree n2 t2s) = 
--  (RTree n1 t1s', RTree n2 t2s') 
--  where 
--    -- I'm not completely clear in my mind but this seems to work?
--    (t1s',t2s') = unzip [(t1,t2) | t1 <- t1s, t2 <- t2s,
--                                   n1 /= n2 || t1 /= t2,
--                                   (t1,t2) `elem` as]

-- | Custom version of GF-UD's matchesUDPattern
-- (cf. https://github.com/GrammaticalFramework/gf-ud/blob/1a4a8c1ac08c02895fa886ca20e5e7a706f484e2/UDPatterns.hs#L23-L27)
-- It differs from the original in that it is nonrecursive  
matchesUDPattern' :: UDPattern -> UDTree -> [UDTree]
matchesUDPattern' p tree@(RTree node subtrees) = case p of
  SEQUENCE ps -> (maybe [] return $ findMatchingUDSequence True ps tree)
  SEQUENCE_ ps -> (maybe [] return $ findMatchingUDSequence False ps tree)
  _ -> [tree | ifMatchUDPattern p tree]

-- | Parses a query string into a list of error patterns, expanding variables
-- and splitting any {X->Y} shorthand. 
parseQuery :: M.Map Field [Value] -> String -> [ErrorPattern]
parseQuery vals q = 
  map (\(q1',q2') -> (read q1',read q2')) (expandVars (q1,q2) exps)
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
                (\(f,ids) -> (f,rmDuplicates ids)) 
                (M.toList $ M.unionWith 
                              (++) 
                              (variables M.empty p1) (variables M.empty p2)
                )

    -- | Expand variables in a sugar-free L1-L2 query
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
  if (head $ strip s) == '$' then M.insertWith (++) "POS" [s] m else m
variables m (DEPREL_ s) =
  if (head $ strip s) == '$' then M.insertWith (++) "DEPREL_" [s] m else m
variables m (FEATS_ s) = M.unionsWith (++) (m:fms)
  where fms = map
                (\d -> let [k,v] = splitOn "=" d in 
                    if (head $ strip v) == '$' 
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
        