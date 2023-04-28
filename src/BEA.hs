{-|
Module      : BEA
Description : Script to run incorrect example retrieval experiments for BEA.
              Usage: bea-exeperiments | bea-experiments md | 
              bea-experiments md-debug
Stability   : experimental
-}

module BEA where

import Data.List
import Data.Bifunctor
import System.Environment (getArgs)
import Control.Monad (when, unless)
import Markdown
import UDConcepts
import Errors
import Align
import Extract
import Match
import Utils.Misc
import Utils.UDPatterns
import Utils.Output


treebankPaths :: (FilePath,FilePath)
treebankPaths = (
  "/home/harisont/Repos/harisont/L1-L2-DaLAJ/M+S/bea/tb_L1.conllu",
  "/home/harisont/Repos/harisont/L1-L2-DaLAJ/M+S/bea/tb_L2.conllu")

examplesPaths :: (FilePath,FilePath)
examplesPaths = (
  "/home/harisont/Repos/harisont/L1-L2-DaLAJ/M+S/bea/ex_L1.conllu",
  "/home/harisont/Repos/harisont/L1-L2-DaLAJ/M+S/bea/ex_L2.conllu")

main = do
  argv <- getArgs
  -- align treebank
  treebank <- parseL1L2treebank treebankPaths
  let alignments = map align treebank 
  -- extract patterns from example sentences
  examples <- parseL1L2treebank examplesPaths
  let errors = map (extract . align) examples
  let mspatterns = 
        map 
          (concatMap (map simplifieduMorphosynErrorPattern . error2patterns)) 
          errors
  let spatterns = map (map uSynErrorPattern) mspatterns
  let ppatterns = map (map uPOSErrorPattern) mspatterns
  let patterns = map 
                  (rmDuplicates . filter (\(p1,p2) -> p1 /= p2))
                  (zipWith (++) (zipWith (++) mspatterns spatterns) ppatterns) 
  -- query the treebank and show the results bc I'm a bad haskeller
  mapM_ 
    (\(e@(e1,e2),ps) -> do
      unless (null argv) $ do -- produce markdown report
        putStrLn $ h2 $ "Input sentence (" ++ showIds e ++ ")" 
        putStrLn $ ulist 0 [
          "L1: " ++ lin e1, 
          "L2: " ++ lin e2
          ]
        when (argv == ["md-debug"]) $ do
          putStrLn $ h3 "Patterns"
          putStrLn $ ulist 0 (map (code . showErrorPattern) ps) 
      let ms = filter 
            (not . null . snd) 
            (treebank `zip` (map 
                              (rmDuplicates . filter (\(m1,m2) -> m1 /= m2)) 
                              (map (match ps) alignments)))
      if null argv
        then do
          let e' = (udSentence2tree e1,udSentence2tree e2)
          let ms' = rmDuplicates $ map ((\s@(s1,s2) -> (showIds s,(udSentence2tree s1,udSentence2tree s2))) . fst) ms
          mapM_ (putStrLn . (\m -> intercalate "\t" [showIds e, linearizeError e', fst m, linearizeError $ snd m])) ms'
          putStrLn ""
        else do -- produce markdown report
          putStrLn $ h3 "Similar examples"
          if null ms 
            then putStrLn "No matches."
            else mapM_ putStrLn (rmDuplicates $ map match2md ms))
    (examples `zip` patterns)
  