module Main where

import Data.Maybe
import Data.Bifunctor
import System.FilePath
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Directory
import Control.Monad (when)
import UDConcepts
import UDPatterns
import Align
import Extract
import Match
import Errors
import Annotate
import Utils.Misc
import Utils.Output
import Utils.UDConcepts
import Utils.UDPatterns

main = do
  argv <- getArgs
  let (flags,args) = parseArgv argv usage opts
  if Help `elem` flags || length args < 3 || head args `notElem` cmds
    then putStrLn $ usageInfo usage opts
    else do
      -- TODO: refactor w/ parseUDtreebank
      s1s <- parseUDFile (args !! 1)
      s2s <- parseUDFile (args !! 2)
      let ids = map sentId s1s `zip` map sentId s2s
      let s12s = s1s `zip` s2s
      -- align sentences
      let as = map align s12s 
      case head args of
        "match" -> do
          -- read query strings from text file or command line
          isFile <- doesFileExist $ args !! 3
          qs <- if length args == 4 && isFile
            then do
              content <- readFile $ args !! 3
              return $ lines content
            else return $ drop 3 args
          -- convert query strings into error patterns
          let ps = rmDuplicates $ concatMap (parseQuery fieldVals) qs
          -- get matches, i.e. pairs of 
          -- (l1-l2 sentences, nonempty list of aligned matching subtrees)
          let ms = filter (not . null . snd) (s12s `zip` map (match ps) as)
          if Markdown `elem` flags
            then mapM_ (putStrLn . match2md) ms
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
              let ses = s12s `zip` ess
              let seps = map 
                    (second 
                      (map (\e -> (e,map 
                              simplifieduMorphosynErrorPattern 
                              (error2patterns e))))
                    ) 
                    ses
              let seps' = filter
                    (not . null . snd)
                    (map 
                      (second (map (second (filter (\(p1,p2) -> p1 /= p2))))) 
                      seps)
              mapM_ (putStrLn . extract2md) seps'
            else do
              let ps = simpler (concat ess)
              mapM_ (putStrLn . showErrorPattern) ps
          case [f | f@CoNNLU {} <- flags] of
            [CoNNLU path] -> do -- no conversion to patterns
              let es = concat ess
              writeFile (path </> "L1.conllu") (conlluText (map fst es))
              writeFile (path </> "L2.conllu") (conlluText (map snd es))
            _ -> return ()
        "example" -> do
          -- annotate
          let lang = args !! 5 
          s1 <- annotate (args !! 3) lang
          s2 <- annotate (args !! 4) lang
          when (Verbose `elem` flags)
            $ do putStrLn $ showUDSentence (1,s1)
                 putStrLn $ showUDSentence (2,s2)
          -- extract error patterns
          let es = extract (align (s1,s2))
          let ps = rmDuplicates $ filter 
                (\(p1,p2) -> p1 /= p2) 
                (patterns es ++ simple es ++ simpler es ++ simplest es)
          when (Verbose `elem` flags) $ mapM_ print ps -- TODO: why does it not print?
          -- query the treebank
          let ms = filter (not . null . snd) (s12s `zip` map (match ps) as)
          let mds = rmDuplicates (map match2md ms)
          mapM_ putStrLn mds
  where 
    patterns = concatMap error2patterns
    simple es = map uMorphosynErrorPattern (patterns es)
    simpler es = map simplifieduMorphosynErrorPattern (patterns es)
    simplest es = map 
      (\(p1,p2) -> (
        (filterUDPattern ["POS", "DEPREL"]) p1, 
        (filterUDPattern ["POS", "DEPREL"]) p2))
      (simpler es)

-- COMMAND LINE OPTIONS PARSING

type Arg = String

data Flag = Help | Markdown | CoNNLU String | Verbose deriving Eq

-- | List of available commands (first arg)
cmds :: [Arg]
cmds = ["extract", "match", "example"]

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
    "path to the directory for the output conllu files",
  Option
    ['v']
    ["verbose"]
    (NoArg Verbose)
    "show intermediate results"
  ]
  where conlluOutDir = CoNNLU . fromMaybe "." -- default = current folder

usage :: String
usage = concat [
  "\nUsage:\n",
  "l2-ud match L1-TREEBANK L2-TREEBANK PATTERNS [OPTIONS], or\n",
  "l2-ud extract L1-TREEBANK L2-TREEBANK [OPTIONS], or\n",
  "l2-ud example L1-TREEBANK L2-TREEBANK L1-SENTENCE L2-SENTENCE LANGUAGE [OPTIONS]"]

parseArgv :: [String] -> String -> [OptDescr Flag] -> ([Flag],[Arg])
parseArgv argv usage opts = case getOpt Permute opts argv of
  (flags,args,[]) -> (flags,args)
  (_,_,errs) -> error $ concat errs ++ usage

