module Output where

import UDConcepts
import Align
import Errors
import Markdown
import Utils

-- | Show the ID(s) of two parallel sentences
showIds :: (UDSentence,UDSentence) -> String
showIds (s1,s2) = if i1 == i2 then i1 else i1 ++ "-" ++ i2
  where (i1,i2) = (sentId s1,sentId s2)

-- | Linearize a sentence highlighting its tokens that belong to a subsentence
highlin :: UDSentence -> UDSentence -> String
highlin s s' = 
  unwords $ map (\w -> if w `elem` wss then bold (udFORM w) else udFORM w) ws
  where 
    ws = udWords s
    wss = udWords s'
    
-- | Render extracted errors/patterns as markdown
extractedErrs2md :: ((UDSentence,UDSentence),[Error]) -> String
extractedErrs2md (s12@(s1,s2),es) = unlines [
  h2 $ "Sentence " ++ showIds s12 ++ ":",
  table 
    ["L1 sentence", "L2 sentence", "L1 pattern", "L2 pattern"]
    (map 
      (\e@(t1,t2) -> let (p1,p2) = error2Pattern e in [
        highlin s1 (udTree2sentence t1), 
        highlin s2 (udTree2sentence t2), 
        code $ show p1, 
        code $ show p2]) 
      es)
  ] 

-- | Render matches as markdown
sentMatches2md :: ((UDSentence,UDSentence),[Alignment]) -> String
sentMatches2md (s12@(s1,s2),as) = unlines [
  h2 $ "Sentence " ++ showIds s12 ++ ":",
  table 
    ["L1 sentence", "L2 sentence"]
    (map 
      (\(t1,t2) -> [
        highlin s1 (udTree2sentence t1), 
        highlin s2 (udTree2sentence t2)]) 
      as)
  ]

-- | Return the string to write in the CoNNL-U file corresponding to a list of 
-- UD trees 
conlluText :: [UDTree] -> String
conlluText ts = unlines $ 
  zipWith (curry showUDSentence) [1..] (map udTree2adjustedSentence ts)

-- | Print a UD sentences with metadata. i is the sent_id. Very similar to
-- https://github.com/GrammaticalFramework/gf-ud/blob/1a4a8c1ac08c02895fa886ca20e5e7a706f484e2/UDConcepts.hs#L172-L180
showUDSentence :: (Int,UDSentence) -> String
showUDSentence (i,s) = (prt . addMeta i) s
 where addMeta i u = 
        u { udCommentLines = ("# sent_id = " ++ show i):udCommentLines s }