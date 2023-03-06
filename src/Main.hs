module Main where

import Data.Maybe
import System.FilePath
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Directory
import UDConcepts
import UDPatterns
import Align
import Extract
import Match
import Errors
import Utils.Misc
import Utils.Output
import Utils.UDConcepts

main = do
  argv <- getArgs
  let (flags,args) = parseArgv argv usage opts
  if Help `elem` flags || length args < 3 || head args `notElem` cmds
    then putStrLn $ usageInfo usage opts
    else do
      s1s <- parseUDFile (args !! 1)
      s2s <- parseUDFile (args !! 2)
      let ids = map sentId s1s `zip` map sentId s2s
      let s12s = s1s `zip` s2s
      -- align sentences
      let as = map align s12s 
      case head args of
        "match" -> do
          -- read query from text file or command line
          isFile <- doesFileExist $ args !! 3
          qs <- if length args == 4 && isFile
            then do
              content <- readFile $ args !! 3
              return $ lines content
            else return $ drop 3 args
          -- get matches, i.e. pairs of 
          -- (l1-l2 sentences, nonempty list of aligned matching subtrees)
          let ms = filter 
                    (not . null . snd) 
                    (s12s `zip` map (match fieldVals qs) as)
          if Markdown `elem` flags
            then mapM_ (putStrLn . sentMatches2md) ms
            else mapM_ ((putStrLn . showIds) . fst) ms
          case [f | f@CoNNLU {} <- flags] of
            [CoNNLU path] -> do
              let as = concatMap snd ms
              writeFile (path </> "L1.conllu") (conlluText (map fst as))
              writeFile (path </> "L2.conllu") (conlluText (map snd as))
            _ -> return ()
        "extract" -> do
          let ess = map extract as
          if Markdown `elem` flags
            then do
              let es = filter (not . null . snd) (s12s `zip` ess)
              mapM_ (putStrLn . extractedErrs2md) es
            else do
              let ps = rmDuplicates $ map error2Pattern (concat ess)
              mapM_ (putStrLn . showErrorPattern) ps
          case [f | f@CoNNLU {} <- flags] of
            [CoNNLU path] -> do
              let es = concat ess
              writeFile (path </> "L1.conllu") (conlluText (map fst es))
              writeFile (path </> "L2.conllu") (conlluText (map snd es))
            _ -> return ()

-- COMMAND LINE OPTIONS PARSING

type Arg = String

data Flag = Help | Markdown | CoNNLU String deriving Eq

-- | List of available commands (first arg)
cmds :: [Arg]
cmds = ["extract", "match"]

opts :: [OptDescr Flag]
opts = [
  Option ['h'] ["help"] (NoArg Help) "show this help message and exit",
  Option 
    ['m'] 
    ["markdown"] 
    (NoArg Markdown) 
    "output a markdown report instead of sentence IDs/patterns",
  Option 
    ['c'] 
    ["conllu"] 
    (OptArg conlluOutDir "DIR") 
    "path to the directory for the output conllu files"
  ]
  where conlluOutDir = CoNNLU . fromMaybe "." -- default = current folder

usage :: String
usage = concat [
  "\nUsage:\n",
  "l2-ud extract L1-TREEBANK L2-TREEBANK [OPTIONS], or\n",
  "l2-ud match L1-TREEBANK L2-TREEBANK PATTERNS [OPTIONS]"]

parseArgv :: [String] -> String -> [OptDescr Flag] -> ([Flag],[Arg])
parseArgv argv usage opts = case getOpt Permute opts argv of
  (flags,args,[]) -> (flags,args)
  (_,_,errs) -> error $ concat errs ++ usage

