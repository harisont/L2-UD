module Main where
  
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
          -- (l1-l2 sentence, nonempty list of aligned matching subtrees)
          let ms = filter 
                    (not . null . snd) 
                    (s12s `zip` (map ((flip match) qs) as))
          if Markdown `elem` flags
            then mapM_ putStrLn (map sentMatches2md ms)
            else mapM_ putStrLn (map showIds (map fst ms))
        "extract" -> do
          let ess = map extract as
          if Markdown `elem` flags
            then do
              let es = filter (not . null . snd) (s12s `zip` ess)
              mapM_ putStrLn (map extractedErrs2md es)
            else do
              let ps = rmDuplicates $ map error2Pattern (concat ess)
              mapM_ (putStrLn . showL1L2Pattern) ps

-- COMMAND LINE OPTIONS PARSING

type Arg = String
data Flag = Help | Markdown deriving Eq

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
    "output a markdown report instead of sentence IDs/patterns"
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

-- | Linearize a sentence highlighting the token that belongs to a subsentence
highlin :: UDSentence -> UDSentence -> String
highlin s ss = unwords $ map (\w -> if w `elem` wss then bold (udFORM w) else udFORM w) ws
  where 
    ws = udWordLines s
    wss = udWordLines ss

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
