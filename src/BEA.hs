{-|
Module      : BEA
Description : Script to run incorrect example retrieval experiments for BEA
Stability   : experimental
-}

module BEA where

import Markdown
import Errors
import Align
import Extract
import Match
import Utils.Misc
import Utils.UDPatterns
import Utils.Output


treebankPaths :: (FilePath,FilePath)
treebankPaths = (
    "/home/harisont/Repos/harisont/L1-L2-DaLAJ/bea_dev/dev_L1.conllu",
    "/home/harisont/Repos/harisont/L1-L2-DaLAJ/bea_dev/dev_L2.conllu")

examplesPaths :: (FilePath,FilePath)
examplesPaths = (
    "/home/harisont/Repos/harisont/L1-L2-DaLAJ/bea_test/test_L1.conllu",
    "/home/harisont/Repos/harisont/L1-L2-DaLAJ/bea_test/test_L2.conllu")

main = do
    -- align treebank
    treebank <- parseL1L2treebank treebankPaths
    let alignments = map align treebank 
    -- extract patterns from example sentences
    examples <- parseL1L2treebank examplesPaths
    let errors = map (extract . align) examples
    let patterns = map 
                    (rmDuplicates . filter 
                                        (\(p1,p2) -> p1 /= p2)
                                  . concatMap (
                                         map simplifieduMorphosynErrorPattern 
                                         . error2patterns)) 
                    errors
    -- query the treebank and show the results bc I'm a bad haskeller
    mapM_ 
        (\(e@(e1,e2),ps) -> do
            putStrLn $ h2 $ "Sentence " ++ showIds e 
            putStrLn $ h3 "Text"
            putStrLn $ ulist 0 [
                "L1: " ++ lin e1, 
                "L2: " ++ lin e2
                ]
            putStrLn $ h3 "Patterns"
            putStrLn $ ulist 0 (map (code . showErrorPattern) ps) 
            let ms = filter 
                        (not . null . snd) 
                        (treebank `zip` map (match ps) alignments)
            putStrLn $ h3 "Similar examples"
            if null ms 
                then putStrLn "No matches."
                else mapM_ putStrLn (rmDuplicates $ map match2md ms))
        (examples `zip` patterns)
    
    
    
    --let matches = filter (not . null . snd) (treebank `zip` filter (not . null) (map (\ps -> filter (not . null) (map (match ps) alignments)) patterns))
    ---- output
    --mapM_ 
    --    (\(ex,ps,(s,ms)) -> 
    --        putStrLn $ 
    --            (example2md (ex,ps)) ++ (unlines $ rmDuplicates $ map (\m -> match2md (s,m)) ms)) 
    --    (zip3 examples patterns matches)