{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Applicative
import           Data.Aeson
import           Search.ElasticSearch

import qualified Data.Text            as T

data Tweet = Tweet String String
           deriving (Show)

instance Document Tweet where
  documentKey _ = T.pack $ show (5::Integer)
  documentType = DocumentType "tweet"

instance ToJSON Tweet where
  toJSON (Tweet t u) = object [ "tweet" .= t
                              , "user" .= u ]

instance FromJSON Tweet where
  parseJSON (Object o) = Tweet <$> o .: "tweet"
                               <*> o .: "user"
  parseJSON _          = error "Not an object"

main :: IO ()
main = do
  let twitterIndex = "twitter"
      tweet = Tweet "Hello world!" "Ollie"
  indexDocument localServer twitterIndex tweet
  docs <- search localServer twitterIndex 0 "user:ollie" :: IO (SearchResults Tweet)
  print docs
