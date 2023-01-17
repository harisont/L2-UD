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
import Errors

main = do
  argv <- getArgs
  let (flags,args) = parseArgv argv usage opts
  if Help `elem` flags || length args < 3 || head args `notElem` cmds
    then putStrLn $ usageInfo usage opts
    else do
      createDirectoryIfMissing True "out" -- just in case it's needed later
      s1s <- parseUDFile (args !! 1)
      s2s <- parseUDFile (args !! 2)
      let ids = map sentId s1s `zip` map sentId s2s
      -- align sentences
      let as = map align (zip s1s s2s) 
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
          let ms = match (concat as) qs 
          if Linearize `elem` flags
            then mapM_ (putStrLn . linearizeAlignment) ms
            else do
              let (l1t,l2t) = unzip ms
              writeFile "out/L1.conllu" (unlines $ [prUDSentence n (udTree2sentence t) | (n, t) <- [1 .. ] `zip` l1t])
              writeFile "out/L2.conllu" (unlines $ [prUDSentence n (udTree2sentence t) | (n, t) <- [1 .. ] `zip` l2t])
        "extract" -> do
          let ess = map extract as
          if Linearize `elem` flags
            then do
              let lines = concatMap showSentErrors (ess `zip` ids) 
              mapM_ putStrLn lines 
            else do
              let ps = map error2Pattern (concat ess)
              let (p1s,p2s) = unzip ps
              writeFile "out/L1.hst" (unlines $ map show p1s)
              writeFile "out/L2.hst" (unlines $ map show p2s)
  where showSentErrors (es, (i1,i2)) = map (\e -> (if i1 == i2 then i1 else i1 ++ "-" ++ i2) ++ ": " ++ linearizeError e) es

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
