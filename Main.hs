module Main where
  
import qualified Data.Map as M
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Directory
import UDConcepts
import UDPatterns
import Align
import Extract
import Match
import Utils

main = do
  argv <- getArgs
  let (flags,args) = parseArgv argv usage opts
  if Help `elem` flags || length args < 3 || head args `notElem` cmds
    then putStrLn $ usageInfo usage opts
    else do
      createDirectoryIfMissing True "out" -- just in case it's needed later
      s1s <- parseUDFile (args !! 1)
      s2s <- parseUDFile (args !! 2)
      -- align sentences
      let as = align s1s s2s 
      case head args of
        "match" -> do
          -- read query from text file or command line
          isFile <- doesFileExist $ args !! 3
          qs <- if length args == 4 && isFile
            then do
              content <- readFile $ args !! 3
              return $ lines content
            else return $ drop 3 args
          -- get matching alignments
          -- TODO: there should be a way to retreive the full sentences too
          let matches = match as qs 
          if Linearize `elem` flags
            then mapM_ (putStrLn . linearizeMatch) matches
            else do
              let (l1s,l2s) = unzip matches
              writeFile "out/L1.conllu" (unlines $ [prUDSentence n s | (n, s) <- [1 .. ] `zip` l1s])
              writeFile "out/L2.conllu" (unlines $ [prUDSentence n s | (n, s) <- [1 .. ] `zip` l2s])
        "extract" -> undefined

-- COMMAND LINE OPTIONS PARSING

type Arg = String
data Flag = Help | Linearize deriving Eq

-- | List of available commands (first arg)
cmds :: [Arg]
cmds = ["extract", "match"]

opts :: [OptDescr Flag]
opts = [
  Option ['h'] ["help"] (NoArg Help) "show this help message and exit",
  Option ['l'] ["linearize"] (NoArg Linearize) "output linearizations instead of CoNNL-U sentences"
  ]

usage :: String
usage = concat [
  "\nUsage:\n",
  "stack run -- extract L1-TREEBANK L2-TREEBANK, or\n",
  "stack run -- match L1-TREEBANK L2-TREEBANK PATTERNS [--linearize]"]

parseArgv :: [String] -> String -> [OptDescr Flag] -> ([Flag],[Arg])
parseArgv argv usage opts = case getOpt Permute opts argv of
  (flags,args,[]) -> (flags,args)
  (_,_,errs) -> error $ concat errs ++ usage
