module Main where
  
import qualified Data.Map as M
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Directory
-- gf-ud
import UDConcepts
import UDPatterns
-- concept-alignment
import ConceptAlignment (alignment2sentencePair) -- TODO: rm when no longer necessary
-- local
import Align
import Match

main = do
  argv <- getArgs
  let (flags,args) = parseArgv argv usage opts
  if Help `elem` flags || length args < 3 
    then putStrLn $ usageInfo usage opts
    else do
      createDirectoryIfMissing True "out" -- just in case it's needed later
      l1Sentences <- parseUDFile (args !! 1)
      l2Sentences <- parseUDFile (args !! 2)
      -- align sentences
      let alignments = defaultAlign l1Sentences l2Sentences 
      case head args of
        "match" -> do
          -- read query from text file or command line
          isFile <- doesFileExist $ args !! 3
          queries <- if length args == 4 && isFile
            then do
              content <- readFile $ args !! 3
              return $ lines content
            else return $ drop 3 args
          -- get matching alignments
          -- TODO: there should be a way to retreive the full sentences too
          let matches = patternMatch alignments queries 
          if Linearize `elem` flags
            then mapM_ (putStrLn . prettyPrintAlignment) matches
            else do
              let (l1s, l2s) = unzip $ map alignment2sentencePair matches
              writeFile "out/L1.conllu" (unlines $ [prUDSentence n s | (n, s) <- [1 .. ] `zip` l1s])
              writeFile "out/L2.conllu" (unlines $ [prUDSentence n s | (n, s) <- [1 .. ] `zip` l2s])
        "extract" -> undefined
        _ -> do 
          putStrLn "The first argument should be an L2-UD command."
          putStrLn "Available commands are: extract, match."

-- COMMAND LINE OPTIONS PARSING
  
data Flag = Help | Linearize deriving Eq
type Arg = String

opts :: [OptDescr Flag]
opts = [
  Option ['h'] ["help"] (NoArg Help) "show this help message and exit",
  Option ['l'] ["linearize"] (NoArg Linearize) "output linearizations instead of CoNNL-U sentences"
  ]

usage :: String
usage = concat [
  "stack run -- extract PATH-TO-L1-TREEBANK PATH-TO-L2-TREEBANK, or\n"
  "stack run -- match PATH-TO-L1-TREEBANK PATH-TO-L2-TREEBANK PATTERNS [--linearize]"]

parseArgv :: [String] -> String -> [OptDescr Flag] -> ([Flag],[Arg])
parseArgv argv usage opts = case getOpt Permute opts argv of
  (flags,args,[]) -> (flags,args)
  (_,_,errs) -> error $ concat errs ++ usage

-- OUTPUT
-- | Temp pretty printer for L1-L2 alignments
-- TODO: replace with something Intro to Programming in Python-inspired
prettyPrintAlignment :: Alignment -> String
prettyPrintAlignment a = mark ++ " (\"" ++ lt ++ "\", \"" ++ lu ++ "\")"
  where 
    (AT (t,u)) = trees a
    (lt,lu) = (linearize t, linearize u)
    mark = if hasGrammError a 
        then "**" 
        else if hasError a then "* " else "  "