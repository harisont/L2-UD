{-|
Module      : Annotate
Description : UD annotation via the UDPipe2 REST API.
Stability   : experimental
-}

{-# LANGUAGE DeriveGeneric #-}

module Annotate where

import GHC.Generics
import Data.ByteString.Lazy.UTF8 (fromString)
import Network.Curl
import Data.Aeson
import UDConcepts

type Sentence = String
type Model = String

data UDPipe2Response = UDPipe2Response {
  model :: String,
  acknowledgements :: [String],
  result :: String
} deriving (Generic, Show)

instance ToJSON UDPipe2Response where
instance FromJSON UDPipe2Response where


annotate :: Sentence -> Model -> IO UDSentence
annotate s m = do
  (code,str) <- curlGetString
    "http://lindat.mff.cuni.cz/services/udpipe/api/process"
    [CurlPost True, CurlPostFields fields]
  if code /= CurlOK
    then error $ "Failed to use the UDPipe 2 API. curl code: " ++ show code
    else do
      case decode (fromString str) :: Maybe UDPipe2Response of
        Nothing -> 
          error $ "Got a malformed response string from UDPipe 2: " ++ str
        Just json -> do
          let ss = parseUDText (result json)
          case length ss of
            0 -> error "Empty input sentence(s)!"
            1 -> return $ head ss
            _ -> error "Input consists of multiple sentences!"
  where
    fields = ["tokenizer=", "tagger=", "parser=", "model=" ++ m, "data=" ++ s]
  