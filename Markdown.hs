-- TODO: make this a separate Haskell package
module Markdown where

import Data.List
import Data.List.Extra

bold :: String -> String
bold s = "**" ++ s ++ "**"

italics :: String -> String
italics s = "_" ++ s ++ "_"

code :: String -> String
code s = "`" ++ s ++ "`"

h1 :: String -> String
h1 s = "# " ++ s

h2 :: String -> String
h2 s = "## " ++ s

table :: [String] -> [[String]] -> String
table h rs = header h ++ unlines (map row rs)
  where 
    header :: [String] -> String
    header cs = unlines [
      row cs,
      row (replicate (length cs) "---")
      ]

    row :: [String] -> String
    row cs = concat $ intersperse " | " (map cell cs)

    cell :: String -> String
    cell s = replace "|" "\\|" s