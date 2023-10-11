{-|
Module      : GUI
Description : A quick-and-dirty GUI for L2-UD.
Stability   : experimental
-}

module GUI where 
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import UDConcepts
import Utils.Output (lin)

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
  element l1Input # set
    (UI.attr "placeholder")
    ("path to L1 treebank")
  element l1Input # set (UI.attr "size") "50%"

  l2Input <- UI.input
  element l2Input # set
    (UI.attr "placeholder")
    ("path to L1 treebank")
  element l2Input # set (UI.attr "size") "50%"

  loadButton <- UI.button
  element loadButton # set UI.text "load"

  break <- UI.br

  queryInput <- UI.input
  element queryInput # set
    (UI.attr "placeholder")
    ("single-language or L1-L2 query")
  element queryInput # set (UI.attr "size") "100%"

  searchButton <- UI.button
  element searchButton # set UI.text "search"

  getBody window #+ [element l1Input, 
                element l2Input,
                element loadButton,
                element break, 
                element queryInput, 
                element searchButton] 
  
  -- !?!?!?!?!?!?!?
  l1Path <- get value l1Input 
  l2Path <- get value l2Input

  on UI.click loadButton $ const $ loadTreebank window l1Path l2Path
  on UI.click searchButton $ undefined

-- | Init table with L1-L2 treebank
loadTreebank :: Window -> FilePath -> FilePath -> UI Element
loadTreebank window path1 path2 = do
  -- TODO: check if the paths are valid

  l1Sents <- liftIO $ parseUDFile path1
  l2Sents <- liftIO $ parseUDFile path2 
  
  table <- buildTable window (map lin l1Sents) (map lin l2Sents)
  getBody window #+ [element table]

buildTable :: Window -> [String] -> [String] -> UI Element
buildTable window l1Data l2Data = do 
  cells <- mapM (mapM (return . string)) (zipWith (\s1 s2 -> [s1,s2]) l1Data l2Data)
  table <- UI.grid cells
  return table