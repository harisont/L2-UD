module Match where 

import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.List.Split (splitOn)
import Data.List.Extra (replace, anySame)
import qualified Text.Regex.Posix as R
import RTree
import UDConcepts
import UDPatterns hiding (matchesUDPattern)
import ConceptAlignment (udSimpleDEPREL)
import Align
import Errors
import Utils

-- | Top-level pattern matching function used in the main
match :: M.Map Field [Value] -> [String] -> [Alignment] -> [Alignment]
match vals qs as = minimal $ concatMap (\a -> concatMap (matches a) ps) as 
  where 
    ps = concatMap (parseQuery vals) qs 
    -- | Checks whether an alignment matches a particular error pattern. If it
    -- does, it returns the portions of the two aligned subtrees actually 
    -- matching the pattern.
    -- Note that checking if an alignment matches an error pattern crucially 
    -- implies to make sure that the matching portions are actually aligned 
    -- with each other, meaning that their subtrees should belong to a 
    -- (smaller) alignment obtained previously
    matches :: Alignment -> ErrorPattern -> [Alignment]
    matches (t1,t2) (p1,p2) = [pruneAlignment (m1,m2) | m1 <- m1s, m2 <- m2s, 
                                              m1 `aligned` m2]
      where
        pruneAlignment (RTree n1 t1s,RTree n2 t2s) = 
          (RTree n1 t1s', RTree n2 t2s') 
          where 
            -- I'm not completely clear in my mind but this seems to work?
            (t1s',t2s') = unzip [(t1,t2) | t1 <- t1s, t2 <- t2s,
                                           n1 /= n2 || t1 /= t2,
                                           (t1,t2) `elem` as]
        (m1s,m2s) = (matchesUDPattern p1 t1,matchesUDPattern p2 t2)
        m1 `aligned` m2 = 
          -- using or because if something is aligned it already means we
          -- are on the right track. No need to check that all subtrees are
          -- aligned with each other
          null t1s || null t2s || or [(t1,t2) `elem` as | t1 <- t1s, 
                                                          t2 <- t2s]
          where (t1s,t2s) = (subtrees m1,subtrees m2)

-- | Custom version of GF-UD's matchesUDPattern
-- (cf. https://github.com/GrammaticalFramework/gf-ud/blob/1a4a8c1ac08c02895fa886ca20e5e7a706f484e2/UDPatterns.hs#L23-L27)
-- - non-recursive (i.e. it only returns the trees matching a pattern 
-- without considering the subtrees) 
-- - prunes the matching trees
matchesUDPattern :: UDPattern -> UDTree -> [UDTree]
matchesUDPattern p tree@(RTree node subtrees) = case p of
  SEQUENCE ps -> 
    map 
      (pruneWithPattern p) 
      (maybe [] return $ findMatchingUDSequence True ps tree)
  SEQUENCE_ ps -> 
    map 
      (pruneWithPattern p) 
      (maybe [] return $ findMatchingUDSequence False ps tree)
  _ -> [pruneWithPattern p tree | ifMatchUDPattern p tree]
  where
    -- | Of a tree matching a pattern, only keep the portion that is actually
    -- involved in the match by filtering subtrees
    -- TODO: should this be recursive, actually?
    pruneWithPattern :: UDPattern -> UDTree -> UDTree
    pruneWithPattern p t = case p of
      (TREE p ps) -> filterSubtrees t p ps
      (TREE_ p ps) -> filterSubtrees t p ps
      (SEQUENCE ps) -> filterTokens t ps 
      (SEQUENCE_ ps) -> filterTokens t ps
      _ -> t
      where
        filterTokens t = filterSubtrees t TRUE -- very hacky but hey
        filterSubtrees t p ps = fst $ replacementsWithUDPattern r t
          where r = FILTER_SUBTREES p (OR ps)

-- | Parses a query string into a list of error patterns, expanding variables
-- and splitting any {X->Y} shorthand. 
parseQuery :: M.Map Field [Value] -> String -> [ErrorPattern]
parseQuery vals q = 
  map (\(q1',q2') -> (read q1',read q2')) (expandVars (q1,q2) exps)
  where 
    -- there's a lot of read and show so both should be at hand :()
    (p1,p2) = (read (desugar q head),read (desugar q last))
    (q1,q2) = (show p1,show p2)

    -- would be incomprehensible even with a type annotation
    -- but basically, if f == head it returns the L1 component of s, 
    --                if f == last it returns the L2 component of s
    desugar s f = case s R.=~ "\\{([^}]*)\\}" :: (String,String,String) of
      (before,"",after) -> s
      -- head and tail remove {}
      (before,match,after) ->
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
  if head s == '$' then M.insertWith (++) "POS" [s] m else m
variables m (DEPREL_ s) =
  if head s == '$' then M.insertWith (++) "DEPREL_" [s] m else m
variables m (FEATS_ s) = M.unionsWith (++) (m:fms)
  where fms = map
                (\d -> let [k,v] = splitOn "=" d in 
                    if head v == '$' 
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
        