module Main where

import Data.Maybe
import Data.Bifunctor
import System.FilePath
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Directory
import Control.Monad (when)
import Markdown
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
  let nullReplacement = CHANGES []
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
          -- read query strings from text file or command line
          isQueryFile <- doesFileExist $ args !! 3
          qs <- if length args == 4 && isQueryFile
            then do
              content <- readFile $ args !! 3
              return $ lines content
            else return $ drop 3 args
          -- convert query strings into error patterns
          let ps = rmDuplicates $ concatMap (parseQuery fieldVals) qs
          -- get matches, i.e. pairs of 
          -- (l1-l2 sentences, nonempty list of aligned matching subtrees)
          r <- case [f | f@Replacement {} <- flags] of
            [Replacement rule] -> do
              isFileReplacement <- doesFileExist rule
              if isFileReplacement 
                then do
                  content <- readFile rule
                  return $ read content
                else return $ read rule  
            _ -> return nullReplacement
          let ms = filter (not . null . snd) (s12s `zip` map (match ps) as)
          if Markdown `elem` flags
            then mapM_ (putStrLn . match2md) ms
            else mapM_ ((putStrLn . showIds) . fst) ms
          case [f | f@CoNNLU {} <- flags] of
            [CoNNLU path] -> do
              let as = concatMap snd ms
              writeFile 
                (path </> "L1.conllu") 
                (conlluTxt (map (fst . replacementsWithUDPattern r . fst) as))
              writeFile 
                (path </> "L2.conllu") 
                (conlluTxt (map (fst . replacementsWithUDPattern r . snd) as))
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
              writeFile (path </> "L1.conllu") (conlluTxt (map fst es))
              writeFile (path </> "L2.conllu") (conlluTxt (map snd es))
            _ -> return ()

        "example" -> do
          -- annotate
          let lang = args !! 5 
          s1 <- annotate (args !! 3) lang
          s2 <- annotate (args !! 4) lang
          when (Verbose `elem` flags)
            $ do let (ss1,ss2) = (showUDSentence (1,s1),showUDSentence (2,s2))
                 if Markdown `elem` flags
                 then do putStrLn $ h2 "UD parses"
                         putStrLn $ codeblock "" ss1
                         putStrLn $ codeblock "" ss2
                 else do putStrLn ss1
                         putStrLn ss2
                         putStrLn ""
          -- extract error patterns
          let es = extract (align (s1,s2))
          let ps = rmDuplicates $ filter 
                (\(p1,p2) -> p1 /= p2) 
                (patterns es ++ simple es ++ simpler es ++ simplest es)
          when (Verbose `elem` flags) 
            $ if Markdown `elem` flags
                then do
                  putStrLn $ h2 "Extracted patterns"
                  putStrLn $ ulist 0 (map (code . showErrorPattern) ps)
                else do 
                  mapM_ (putStrLn . showErrorPattern) ps
                  putStrLn ""
          -- query the treebank
          let ms = filter (not . null . snd) (s12s `zip` map (match ps) as)
          let mds = rmDuplicates (map match2md ms)
          if Markdown `elem` flags
            then mapM_ putStrLn mds
            else mapM_ ((putStrLn . showIds) . fst) ms
  where 
    patterns = concatMap error2patterns
    simple es = map uMorphosynErrorPattern (patterns es)
    simpler es = map simplifieduMorphosynErrorPattern (patterns es)
    simplest es = map uSynErrorPattern (simpler es)


-- | Command-line arguments
type Arg = String

-- | Flags
data Flag = 
    Help
  | Markdown
  | CoNNLU String 
  | Verbose 
  | Replacement String 
  deriving Eq

-- | List of available commands (first arg)
cmds :: [Arg]
cmds = ["extract", "match", "example"]

-- | Command-line options with descriptions
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
    "show intermediate results",
  Option
    ['r']
    ["replacement-rule"]
    (ReqArg Replacement "RULE_OR_PATH")
    "apply a custom replacement rule on all trees"
  ]
  where conlluOutDir = CoNNLU . fromMaybe "." -- default = current folder

usage :: String
usage = concat [
  "\nUsage:\n",
  "l2-ud match L1-TB L2-TB PATTERNS [OPTS], or\n",
  "l2-ud extract L1-TB L2-TB [OPTS], or\n",
  "l2-ud example L1-TB L2-TB L1-SENT L2-SENT LANG [OPTS]"]

parseArgv :: [String] -> String -> [OptDescr Flag] -> ([Flag],[Arg])
parseArgv argv usage opts = case getOpt Permute opts argv of
  (flags,args,[]) -> (flags,args)
  (_,_,errs) -> error $ concat errs ++ usage

