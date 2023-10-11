{-|
Module      : GUI
Description : A quick-and-dirty GUI for L2-UD.
Stability   : experimental
-}

module GUI where 
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

main :: IO ()
main = do
  startGUI defaultConfig
    { jsPort       = Just 8023
    , jsStatic     = Just "static"
    } setup

setup :: Window -> UI ()
setup w = do
  return w # set UI.title "L2-UD"

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

  getBody w #+ [element l1Input, 
                element l2Input,
                element loadButton,
                element break, 
                element queryInput, 
                element searchButton] 

  on UI.click searchButton $ undefined
    