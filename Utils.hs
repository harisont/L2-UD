module Utils where

import RTree
import UDConcepts

udTree2adjustedSentence :: UDTree -> UDSentence
udTree2adjustedSentence = adjustUDIds . udTree2sentence . createRoot

rootID :: UDTree -> UDId
rootID (RTree n _) = udID n