module Utils where

import UDConcepts

udTree2adjustedSentence :: UDTree -> UDSentence
udTree2adjustedSentence = adjustUDIds . udTree2sentence . createRoot