{-|
Module      : GUI
Description : A quick-and-dirty GUI for L2-UD.
Stability   : experimental
-}

module GUI where 
import Data.Maybe
import System.Directory
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import UDConcepts
import UDPatterns
import Utils.UDConcepts
import Utils.Output
import Align
import Match
import Errors

data Mode = Text | CoNNLU deriving (Eq, Show)

main :: IO ()
main = do
  startGUI defaultConfig
    { jsPort = Just 8023
    , jsStatic = Just "static"
    } setup

setup :: Window -> UI ()
setup window = do
  return window # set UI.title "L2-UD"

  l1Input <- buildTextInput "path to L1 treebank" "path"
  element l1Input # set UI.style [("width","49.4%")]
  l2Input <- buildTextInput "path to L2 treebank" "path"
  element l2Input # set UI.style [("width","49.4%")]

  queryInput <- buildTextInput
                  "L1-L2 or single-language (matched on L2) query"
                  "query"
  element queryInput # set UI.style [("width","99.2%")]

  searchButton <- UI.button
  element searchButton # set UI.text "search"
  element searchButton # set UI.style [
      ("margin","1%")
    , ("font-family", "arial, sans-serif")
    ]

  replacementInput <- buildTextInput 
                        "additional replacement rule (optional)"
                        "replacement"
  element replacementInput # set UI.style [("width","99.2%")]

  modeSpan <- string "Mode: "
  
  textMode <- buildMode "text" True
  conlluMode <- buildMode "CoNNL-U" False

  getBody window #+ [
                element l1Input
              , element l2Input
              , element queryInput 
              , element replacementInput
              , element modeSpan
              , element textMode
              , element conlluMode
              , element searchButton
                ] 
  
  on UI.click searchButton $ const $ do
    l1Path <- get value l1Input 
    l2Path <- get value l2Input
    queryTxt <- get value queryInput
    replacementTxt <- get value replacementInput
    textModeButton <- getElementById window "text"
    textModeValue <- get UI.checked (fromJust $ textModeButton)
    let mode = if textModeValue then Text else CoNNLU

    l1Exists <- liftIO $ doesFileExist l1Path
    l2Exists <- liftIO $ doesFileExist l2Path
    let queryExists = not $ null queryTxt
    let replacement = if null replacementTxt 
                        then CHANGES [] 
                        else read replacementTxt

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
            let patterns = parseQuery fieldVals queryTxt
            let matches = filter 
                            (not . null . snd) 
                            (treebank `zip` map (match patterns) alignments)
            let matches' = map 
                            (\(s,es) -> 
                              (s,map (applyReplacement replacement) es))
                            matches
            let (l1Col,l2Col) = unzip $ concatMap 
                  (\((s1,s2),ms) -> 
                    map 
                      (\(m1,m2) -> 
                        let m1' = udTree2sentence (adjustRootAndPositions m1)
                            m2' = udTree2sentence (adjustRootAndPositions m2)
                        in ((if mode == Text 
                            then highlin s1 (udTree2sentence m1) HTML
                            else prReducedUDSentence "xxxxxxxx" m1', 
                          if mode == Text 
                            then highlin s2 (udTree2sentence m2) HTML
                            else prReducedUDSentence "xxxxxxxx" m2')
                        )) 
                      ms) 
                  matches'
            table <- buildTable window l1Col l2Col 
            destroyTables window
            getBody window #+ [element table]
          else do
            table <- buildTable window (map lin l1Sents) (map lin l2Sents)
            destroyTables window
            getBody window #+ [element table]
  where applyReplacement r (e1,e2) = 
          (fst $ replacementsWithUDPattern r e1,
           fst $ replacementsWithUDPattern r e2)

type Placeholder = String
type Class = String

buildTextInput :: Placeholder -> Class -> UI Element
buildTextInput p c = do
  input <- UI.input
  element input # set (UI.attr "placeholder") p
  element input # set (UI.attr "class") c
  element input # set UI.style [("font-family", "arial, sans-serif")]
  markRight input
  return input

buildMode :: String -> Bool -> UI Element
buildMode mode checked = do
  span <- UI.span
  radioButton <- UI.input
  label <- UI.label
  element radioButton # set UI.type_ "radio"
  element radioButton # set UI.name "mode"
  element radioButton # set UI.id_ mode
  element radioButton # set UI.checked checked
  element label # set UI.type_ "label"
  element label # set UI.text mode
  element label # set UI.for mode
  element span # set children [radioButton, label]
  element span # set UI.style [("font-family", "arial, sans-serif")]
  return span
      
buildTable :: Window -> [String] -> [String] -> UI Element
buildTable window l1Data l2Data = do 
  cells <- mapM 
            (\(n,row) -> mapM 
              (return . (\htmlText -> do
                div <- UI.div
                element div # set html htmlText
                element div # set UI.style [
                    ("text-align", "left")
                  , ("padding", "8px")
                  , ("white-space", "pre-wrap")
                  , ("background-color", if even n then "#dddddd" else "#ffffff")
                  ]
                return div))
              row)
            ([1..] `zip` zipWith (\s1 s2 -> [s1,s2]) l1Data l2Data)
  table <- UI.grid cells
  element table # set UI.style [
      ("width","100%")
    , ("table-layout","fixed")
    , ("font-family", "arial, sans-serif")
    ]
  return table

destroyTables :: Window -> UI ()
destroyTables window = do
  tables <- getElementsByClassName window "table"
  mapM_ delete tables 

markWrong :: Element -> UI Element
markWrong input = element input # set (UI.attr "bgcolor") ("red")

markRight :: Element -> UI Element
markRight input = element input # set (UI.attr "bgcolor") ("white")