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
import Utils
import Markdown

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
              mapM_ (putStrLn . showL1L2Pattern) ps
          case [f | f@CoNNLU {} <- flags] of
            [CoNNLU path] -> undefined
            _ -> return ()

-- COMMAND LINE OPTIONS PARSING

type Arg = String

data Flag = Help | Markdown | CoNNLU String deriving Eq

conlluOutDir :: Maybe String -> Flag
conlluOutDir = CoNNLU . fromMaybe "." -- default = current folder

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

usage :: String
usage = concat [
  "\nUsage:\n",
  "stack run -- extract L1-TREEBANK L2-TREEBANK [--markdown], or\n",
  "stack run -- match L1-TREEBANK L2-TREEBANK PATTERNS [--markdown]"]

parseArgv :: [String] -> String -> [OptDescr Flag] -> ([Flag],[Arg])
parseArgv argv usage opts = case getOpt Permute opts argv of
  (flags,args,[]) -> (flags,args)
  (_,_,errs) -> error $ concat errs ++ usage

-- HELPER FUNCTIONS FOR OUTPUT

-- | Linearize a sentence highlighting its tokens that belong to a subsentence
highlin :: UDSentence -> UDSentence -> String
highlin s s' = 
  unwords $ map (\w -> if w `elem` wss then bold (udFORM w) else udFORM w) ws
  where 
    ws = udWords s
    wss = udWords s'

-- | Return the words a UD sentence is composed of, ignoring unsplit tokens
-- (cf. https://github.com/harisont/L2-UD/issues/12)
udWords :: UDSentence -> [UDWord]
udWords s = filter ((not . isRange) . udID) (udWordLines s)
  where
    isRange :: UDId -> Bool
    isRange (UDIdRange _ _) = True
    isRange _ = False
    
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

-- | Show the ID(s) of two parallel sentences
showIds :: (UDSentence,UDSentence) -> String
showIds (s1,s2) = if i1 == i2 then i1 else i1 ++ "-" ++ i2
  where (i1,i2) = (sentId s1,sentId s2)

-- | Return the string to write in the CoNNL-U file corresponding to a list of 
-- UD trees 
conlluText :: [UDTree] -> String
conlluText ts = 
  unlines $ map showUDSentence ([1..] `zip` map udTree2adjustedSentence ts)

-- | Print a UD sentences with metadata. i is the sent_id. Very similar to
-- https://github.com/GrammaticalFramework/gf-ud/blob/1a4a8c1ac08c02895fa886ca20e5e7a706f484e2/UDConcepts.hs#L172-L180
showUDSentence :: (Int,UDSentence) -> String
showUDSentence (i,s) = (prt . addMeta i) s
 where addMeta i u = 
        u { udCommentLines = ("# sent_id = " ++ show i):udCommentLines s }
