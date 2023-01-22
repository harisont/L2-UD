module Match where 

import Data.List.Split (splitOn)
import qualified Text.Regex.Posix as R
import RTree
import UDConcepts
import UDPatterns hiding (matchesUDPattern)
import Align
import Errors

-- | Top-level pattern matching function used in the main
match :: [Alignment] -> [String] -> [Alignment]
match as qs = minimal $ concatMap (\a -> concatMap (matches a) ps) as 
  where 
    ps = map parseQuery qs
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
  SEQUENCE ps  -> 
    map (pruneWithPattern p) (maybe [] return $ findMatchingUDSequence True ps tree)
  SEQUENCE_ ps  -> 
    map (pruneWithPattern p) (maybe [] return $ findMatchingUDSequence False ps tree)
  _ -> [pruneWithPattern p tree | ifMatchUDPattern p tree]
  where
    -- | Of a tree matching a pattern, only keep the portion that is actually
    -- involved in the match by filtering subtrees
    pruneWithPattern :: UDPattern -> UDTree -> UDTree
    pruneWithPattern p t = case p of
      (TREE p ps) -> filterSubtrees t p ps
      (TREE_ p ps) -> filterSubtrees t p ps
      _ -> t -- TODO: SEQUENCE? 
      where filterSubtrees t p ps = fst $ replacementsWithUDPattern r t
              where r = FILTER_SUBTREES p (OR ps)

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