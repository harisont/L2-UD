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
                                  . map error2simplifiedUniMorphosynPattern) 
                    errors
    -- query the treebank with the examples
    let matches = filter (not . null . snd) (treebank `zip` filter (not . null) (map (\ps -> filter (not . null) (map (match ps) alignments)) patterns))
    -- output
    mapM_ 
        (\(ex,ps,(s,ms)) -> 
            putStrLn $ 
                (example2md (ex,ps)) ++ (unlines $ rmDuplicates $ map (\m -> sentMatches2md (s,m)) ms)) 
        (zip3 examples patterns matches)