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

  -- input field for query
  qInput <- UI.input
  element qInput # set
    (UI.attr "placeholder")
    ("single-language or L1-L2 query")
  element qInput # set (UI.attr "size") "60"

  -- create search button
  sButton <- UI.button
  element sButton # set UI.text "search"

  -- add graphical elements to window body
  getBody w #+ [element qInput, element sButton] 

  on UI.click sButton $ undefined
    