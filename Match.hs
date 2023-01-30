module Match where 

import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.List.Split (splitOn)
import Data.List.Extra (replace)
import qualified Text.Regex.Posix as R
import RTree
import UDConcepts
import UDPatterns hiding (matchesUDPattern)
import ConceptAlignment (udSimpleDEPREL)
import Align
import Errors
import Utils

-- | Top-level pattern matching function used in the main
match :: [Alignment] -> [String] -> [Alignment]
match as qs = error $ show $ ps--minimal $ concatMap (\a -> concatMap (matches a) ps) as 
  where 
    ps = concatMap (parseQuery vals) qs 
    --VALS = (M.fromList [("POS", ["NOUN", "VERB"])])
    vals = M.fromList [
      ("FORM",values udFORM),
      ("LEMMA",values udLEMMA),
      ("POS",values udUPOS),
      ("DEPREL",values udDEPREL),
      ("DEPREL_",values udSimpleDEPREL)
      -- TODO: ("FEATS", values udFEATS),
      -- TODO: add FEATS_
      ]
    values f = take 2 (concatMap (\(t1,t2) -> rmDuplicates $ map f (allNodesRTree t1) ++ map f (allNodesRTree t2)) as) -- TODO: rm take
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
    map (pruneWithPattern p) (maybe [] return $ findMatchingUDSequence True ps tree)
  SEQUENCE_ ps -> 
    map (pruneWithPattern p) (maybe [] return $ findMatchingUDSequence False ps tree)
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

type Field = String -- the name of a CoNNL-U "column" or morphological feature
type Value = String -- the value of a certain "field" 

-- | Parses a query string into a list of error patterns, expanding variables
-- and splitting any {X->Y} shorthand. 
parseQuery :: M.Map Field [Value] -> String -> [ErrorPattern]
parseQuery vals q = map (\(q1',q2') -> (read q1',read q2')) (expandVars (q1,q2) exps)
  where 
    (p1,p2) = (read (splitL1L2 q head),read (splitL1L2 q last))
    (q1,q2) = (show p1,show p2)

    variables m (FORM s) = 
      if head s == '$' then M.insertWith (++) "FORM" [s] m else m
    variables m (LEMMA s) =
      if head s == '$' then M.insertWith (++) "LEMMA" [s] m else m
    variables m (POS s) =
      if head s == '$' then M.insertWith (++) "POS" [s] m else m
    variables m (DEPREL s) =
      if head s == '$' then M.insertWith (++) "DEPREL" [s] m else m
    variables m (DEPREL_ s) =
      if head s == '$' then M.insertWith (++) "DEPREL_" [s] m else m
    variables m (FEATS s) =
      if head s == '$' then M.insertWith (++) "FEATS" [s] m else m
    -- TODO:
    --variables m (FEATS_ s) = 
    --  M.unions $ map 
    --    (\(k,v) -> if head v == "$" 
    --                then M.insertWith (++) ("FEATS_" ++ k) [v] m
    --                else m)
    --    (map (\f -> let [k,v] = splitOn "=" f in (k,v)) (splitOn "|" s))
    variables m (AND ps) = M.unions (map (variables m) ps)
    variables m (OR ps) = M.unions (map (variables m) ps)
    --variables m (ARG pos deprel) = TODO:
    variables m (SEQUENCE ps) = M.unions (map (variables m) ps)
    variables m (SEQUENCE_ ps) = M.unions (map (variables m) ps)
    variables m (NOT p) = variables m p
    variables m (TREE p ps) = 
      M.unions ((variables m p):(map (variables m) ps))
    variables m (TREE_ p ps) = 
      M.unions ((variables m p):(map (variables m) ps))
    variables m _ = m

    splitL1L2 s f = case s R.=~ "\\{([^}]*)\\}" :: (String,String,String) of
      (before,"",after) -> s
      -- head and tail remove {}
      (before,match,after) ->
         before ++ f (splitOn "->" (tail $ init match)) ++ splitL1L2 after f
    
    exps = concatMap (\(f,ids) -> map (\(i) -> (i, fromJust $ M.lookup f vals)) ids) (map (\(f,ids) -> (f,rmDuplicates ids)) (M.toList $ M.unionWith (++) (variables M.empty p1) (variables M.empty p2)))
    
    expandVars (q1,q2) [] = [(q1,q2)]
    expandVars (q1,q2) (e:es) =
      -- TODO: this does NOT work with multiple variables 
      filter (\(a,b) -> q1 == q2 || a /= b) (concatMap ((flip expandVars) es) (expandVar (q1,q2) e))
      where expandVar (q1,q2) (i,vs) = 
              map (\v -> (replace i v q1,replace i v q2)) vs
        