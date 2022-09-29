{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module BasicSchema where

import           Data.Aeson
import           Data.Aeson.Types
import           Database.Persist (Entity(..), Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Topic sql=topics
    name Text
    UniqueTitle name
    deriving Show Read Eq

  Iteam sql=iteams
    title Text
    thumbnail Text
    pages Int
    link Text
    publisher Text
    topicId TopicId
    UniqueText title
    deriving Show Read Eq
|]

instance ToJSON (Entity Iteam) where
  toJSON (Entity bid iteam) = object $
    "id" .= (fromSqlKey bid) : iteamPairs iteam

instance ToJSON Iteam where
  toJSON iteam = object (iteamPairs iteam)

iteamPairs :: Iteam -> [Pair]
iteamPairs iteam =
  [ "title" .= iteamTitle iteam
  , "thumbnail" .= iteamThumbnail iteam
  , "pages" .= iteamPages iteam
  , "link" .= iteamLink iteam
  , "publisher" .= iteamPublisher iteam
  , "topicId" .= iteamTopicId iteam
  ]

instance FromJSON (Entity Iteam) where
  parseJSON = withObject "Iteam Entity" $ \o -> do
    iteam <- parseIteam o
    bid <- o .: "id"
    return $ Entity (toSqlKey bid) iteam


instance FromJSON Iteam where
  parseJSON = withObject "Iteam" parseIteam
  

parseIteam :: Object -> Parser Iteam
parseIteam o = do
  uTitle <- o .: "title"
  uThumbnail <- o .: "thumbnail"
  uPages <- o .: "pages"
  uLink <- o .: "link"
  uPublisher <- o .: "publisher"
  uTopicId <- o .: "topicId"
  return Iteam
    { iteamTitle = uTitle
    , iteamThumbnail = uThumbnail
    , iteamPages = uPages
    , iteamLink = uLink
    , iteamPublisher = uPublisher
    , iteamTopicId = uTopicId
    }


--------------------------------------------------------------
instance ToJSON (Entity Topic) where
  toJSON (Entity lid topic) = object $
    "id" .= (fromSqlKey lid) : topicPairs topic

instance ToJSON Topic where
  toJSON topic = object (topicPairs topic)

topicPairs :: Topic -> [Pair]
topicPairs topic =
  [ "name" .= topicName topic
  ]

instance FromJSON (Entity Topic) where
  parseJSON = withObject "Topic Entity" $ \o -> do
    topic <- parseTopic o
    lid <- o .: "id"
    return $ Entity (toSqlKey lid) topic

--
instance FromJSON Topic where
  parseJSON = withObject "Topic" parseTopic

parseTopic :: Object -> Parser Topic
parseTopic o = do
  uName <- o .: "name"
  return Topic
    { topicName = uName
    }