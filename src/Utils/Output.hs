{-|
Module      : Utils.Output
Description : Output functions used in the main.
Stability   : experimental
-}

module Utils.Output where

import UDConcepts
import Align
import Errors
import Markdown
import Utils.Misc
import Utils.UDConcepts

-- | Show the ID(s) of two parallel sentences
showIds :: (UDSentence,UDSentence) -> String
showIds (s1,s2) = if i1 == i2 then i1 else i1 ++ "-" ++ i2
  where (i1,i2) = (sentId s1,sentId s2)
    
-- | Render extracted errors/patterns as markdown
extract2md :: ((UDSentence,UDSentence),[(Error,ErrorPattern)]) -> String
extract2md (s12@(s1,s2),eps) = unlines [
  h2 $ "Sentence " ++ showIds s12 ++ ":",
  table 
    ["L1 sentence", "L2 sentence", "Error pattern"]
    (map 
      (\(e@(t1,t2),p) -> [
        highlin s1 (udTree2sentence t1), 
        highlin s2 (udTree2sentence t2), 
        code $ showErrorPattern p])
      eps)
  ] 

-- | Render matches as markdown
sentMatches2md :: ((UDSentence,UDSentence),[Alignment]) -> String
sentMatches2md (s12@(s1,s2),as) = unlines [
  h3 $ "Sentence " ++ showIds s12 ++ ":",
  table 
    ["L1 sentence", "L2 sentence"]
    (map 
      (\(t1,t2) -> [
        highlin s1 (udTree2sentence t1), 
        highlin s2 (udTree2sentence t2)]) 
      as)
  ]

-- | Render example sentence pair as markdown
example2md :: ((UDSentence,UDSentence),[ErrorPattern]) -> String
example2md (e@(s1,s2),ps) = unlines [
  h2 $ "Sentence " ++ showIds e ++ ":",
  ulist 1 [highlin s1 s1, highlin s2 s2],
  "",
  "Patterns: ",
  ulist 1 (map (code . showErrorPattern) ps)
  ]

-- | Helper function that linearizes a sentence highlighting its tokens that 
-- belong to a subsentence
highlin :: UDSentence -> UDSentence -> String
highlin s s' = 
  unwords $ map (\w -> if w `elem` wss then bold (udFORM w) else udFORM w) ws
  where 
    ws = udWords s
    wss = udWords s'

-- | Return the string to write in the CoNNL-U file corresponding to a list of 
-- UD trees 
conlluText :: [UDTree] -> String
conlluText ts = unlines $ 
  zipWith (curry showUDSentence) [1..] (map udTree2adjustedSentence ts)

-- | Helper function: print a UD sentences with metadata. i is the sent_id. 
-- Very similar to
-- https://github.com/GrammaticalFramework/gf-ud/blob/1a4a8c1ac08c02895fa886ca20e5e7a706f484e2/UDConcepts.hs#L172-L180
showUDSentence :: (Int,UDSentence) -> String
showUDSentence (i,s) = (prt . addMeta i) s
 where addMeta i u = 
        u { udCommentLines = ("# sent_id = " ++ show i):udCommentLines s }