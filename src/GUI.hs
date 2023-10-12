{-|
Module      : GUI
Description : A quick-and-dirty GUI for L2-UD.
Stability   : experimental
-}

module GUI where 
import System.Directory
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import UDConcepts
import Utils.UDConcepts
import Utils.Output (lin)
import Align
import Match

main :: IO ()
main = do
  startGUI defaultConfig
    { jsPort       = Just 8023
    , jsStatic     = Just "static"
    } setup

setup :: Window -> UI ()
setup window = do
  return window # set UI.title "L2-UD"

  l1Input <- UI.input
  element l1Input # set (UI.attr "placeholder") ("path to L1 treebank")
  element l1Input # set (UI.attr "size") "50%"

  l2Input <- UI.input
  element l2Input # set (UI.attr "placeholder") ("path to L1 treebank")
  element l2Input # set (UI.attr "size") "50%"

  break <- UI.br

  queryInput <- UI.input
  element queryInput # set
    (UI.attr "placeholder")
    ("single-language or L1-L2 query")
  element queryInput # set (UI.attr "size") "100%"

  searchButton <- UI.button
  element searchButton # set UI.text "search"

  getBody window #+ [
                element l1Input, 
                element l2Input,
                element break, 
                element queryInput, 
                element searchButton] 
  
  on UI.click searchButton $ const $ do
    l1Path <- get value l1Input 
    l2Path <- get value l2Input
    queryTxt <- get value queryInput

    l1Exists <- liftIO $ doesFileExist l1Path
    l2Exists <- liftIO $ doesFileExist l2Path
    let queryExists = not $ null queryTxt

    case (l1Exists,l2Exists) of
      (False,False) -> do
        markWrong l1Input
        markWrong l2Input
      (False,True) -> markWrong l1Input
      (True,False) -> markWrong l2Input
      (True,True) -> do
        l1Sents <- liftIO $ parseUDFile l1Path
        l2Sents <- liftIO $ parseUDFile l2Path 
        let treebank = l1Sents `zip` l2Sents 
        let alignments = map align treebank
        if queryExists 
          then do 
            let patterns = parseQuery fieldVals queryTxt
            let matches = filter 
                            (not . null . snd) 
                            (treebank `zip` map (match patterns) alignments)
            -- TODO: allow seeing only matching subtrees
            let (matchingL1Sents,matchingL2Sents) = unzip (map fst matches) 
            table <- buildTable 
                      window 
                      (map lin matchingL1Sents) 
                      (map lin matchingL2Sents)
            destroyTables window
            getBody window #+ [element table]
          else do
            table <- buildTable window (map lin l1Sents) (map lin l2Sents)
            destroyTables window
            getBody window #+ [element table]
    
buildTable :: Window -> [String] -> [String] -> UI Element
buildTable window l1Data l2Data = do 
  cells <- mapM 
            (mapM (return . string)) 
            (zipWith (\s1 s2 -> [s1,s2]) l1Data l2Data)
  table <- UI.grid cells
  return table

destroyTables :: Window -> UI ()
destroyTables window = do
  tables <- getElementsByClassName window "table"
  mapM_ delete tables 

-- | TODO: find out why this only works with DarkReader enabled
markWrong :: Element -> UI Element
markWrong input = element input # set (UI.attr "background") ("red")