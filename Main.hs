module Main where
  
import qualified Data.Map as M
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Directory
-- gf-ud
import UDConcepts
import UDPatterns
-- concept-alignment
import ConceptAlignment
-- local
import L2UD

main = do
  argv <- getArgs
  let (flags,args) = parseArgv argv usage opts
  if Help `elem` flags || length args < 3 
    then putStrLn $ usageInfo usage opts
    else do
      l1Sentences <- parseUDFile (args !! 0)
      l2Sentences <- parseUDFile (args !! 1)
      let alignments = defaultAlign l1Sentences l2Sentences 
      isFile <- doesFileExist $ args !! 2
      queries <- if length args == 3 && isFile
        then do
          content <- readFile $ args !! 2
          return $ lines content
        else return $ drop 2 args
      let errorPatterns = map parseQuery queries
      let matches = concatMap (queryL1L2treebank alignments) errorPatterns
      if Linearize `elem` flags
        then mapM_ (putStrLn . prettyPrintAlignment) matches
        else do
          createDirectoryIfMissing True "out"
          let (l1s, l2s) = unzip $ map alignment2sentencePair matches
          writeFile "out/L1.conllu" (unlines $ [prUDSentence n s | (n, s) <- [1 .. ] `zip` l1s])
          writeFile "out/L2.conllu" (unlines $ [prUDSentence n s | (n, s) <- [1 .. ] `zip` l2s])

-- COMMAND LINE OPTIONS PARSING
  
data Flag = Help | Linearize deriving Eq
type Arg = String

opts :: [OptDescr Flag]
opts = [
  Option ['h'] ["help"] (NoArg Help) "show this help message and exit",
  Option ['l'] ["linearize"] (NoArg Linearize) "output linearizations instead of CoNNL-U sentences"
  ]

usage :: String
usage = "stack run -- PATH-TO-L1-TREEBANK PATH-TO-L2-TREEBANK PATTERNS [--linearize]"

parseArgv :: [String] -> String -> [OptDescr Flag] -> ([Flag],[Arg])
parseArgv argv usage opts = case getOpt Permute opts argv of
  (flags,args,[]) -> (flags,args)
  (_,_,errs) -> error $ concat errs ++ usage