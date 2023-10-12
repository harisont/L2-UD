{-|
Module      : GUI
Description : A quick-and-dirty GUI for L2-UD.
Stability   : experimental
-}

module GUI where 

import Control.Exception
import System.Directory
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import UDConcepts
import Utils.UDConcepts
import Utils.Output (lin)
import Align
import Match
import Errors

main :: IO ()
main = do
  startGUI defaultConfig
    { jsPort = Just 8023
    , jsStatic = Just "static"
    } setup

setup :: Window -> UI ()
setup window = do
  return window # set UI.title "L2-UD"
  UI.addStyleSheet window "style.css"

  l1Input <- UI.input
  element l1Input # set (UI.attr "placeholder") ("path to L1 treebank")
  element l1Input # set (UI.attr "class") "path"
  markRight l1Input

  l2Input <- UI.input
  element l2Input # set (UI.attr "placeholder") ("path to L2 treebank")
  element l2Input # set (UI.attr "class") "path"
  markRight l2Input

  queryInput <- UI.input
  element queryInput # set
    (UI.attr "placeholder")
    ("single-language or L1-L2 query")
  element queryInput # set (UI.attr "id") "query"
  markRight queryInput


  searchButton <- UI.button
  element searchButton # set UI.text "search"

  getBody window #+ [
                element l1Input, 
                element l2Input,
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
      (False,True) -> do
        markWrong l1Input
        markRight l2Input
      (True,False) -> do
        markWrong l2Input
        markRight l1Input
      (True,True) -> do
        markRight l1Input
        markRight l2Input
        l1Sents <- liftIO $ parseUDFile l1Path
        l2Sents <- liftIO $ parseUDFile l2Path 
        let treebank = l1Sents `zip` l2Sents 
        let alignments = map align treebank
        if queryExists 
          then do
            tried <- liftIO $ (try (return $ parseQuery fieldVals queryTxt) :: IO (Either MyException [ErrorPattern])) 
            case tried of
              (Left _) -> markWrong queryInput
              (Right patterns) -> do
                let matches = filter 
                                (not . null . snd) 
                                (treebank `zip` map (match patterns) alignments)
                -- TODO: discard less info to allow seeing only matching subtrees
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

-- TODO: find out why these only work with DarkReader 
-- enabled
markWrong :: Element -> UI Element
markWrong input = element input # set (UI.attr "bgcolor") ("red")

markRight :: Element -> UI Element
markRight input = element input # set (UI.attr "bgcolor") ("white")

data MyException = ThisException | ThatException
    deriving Show

instance Exception MyException