{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Database.Persist hiding ((==.))
import           Database.Persist.Postgresql hiding ((==.))
import           Database.Esqueleto (select, from, where_, (^.), val, (==.), on,
                                     InnerJoin(..), limit, orderBy, desc)

import BasicSchema

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=newsDB password=tanja"

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
   runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False


fetchIteamPG :: PGInfo -> Int64 -> IO (Maybe Iteam)
fetchIteamPG connString bid = runAction connString (get (toSqlKey bid))


fetchAllIteamsForTopicPG :: PGInfo -> Int64  -> IO [Entity Iteam]
fetchAllIteamsForTopicPG connString lid = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Iteam]
    fetchAction = select . from $ \iteams2 -> do
      where_ (iteams2 ^. IteamTopicId ==. val (toSqlKey lid))
      return iteams2


fetchAllIteamsPG :: PGInfo -> IO [Entity Iteam]
fetchAllIteamsPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Iteam]
    fetchAction = select . from $ \iteams2 -> do
      --where_ (articles ^. ArticleAuthorId ==. val (toSqlKey uid))
      return iteams2



fetchRecentIteamsPG :: PGInfo -> IO [(Entity Topic , Entity Iteam)]
fetchRecentIteamsPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity Topic, Entity Iteam)]
    fetchAction = select . from $ \(topics `InnerJoin` iteams) -> do
      on (topics ^. TopicId  ==. iteams ^. IteamTopicId)
      orderBy [desc (iteams ^. IteamPages)]
      limit 10
      return (topics, iteams)



fetchPrekoJoinaPG :: PGInfo -> IO [Entity Iteam]
fetchPrekoJoinaPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Iteam]
    fetchAction = select . from $ \(topics `InnerJoin` iteams) -> do
      on (topics ^. TopicId  ==. iteams ^. IteamTopicId)
      orderBy [desc (iteams ^. IteamPages)]
      limit 10
      return iteams

createIteamPG :: PGInfo -> Iteam -> IO Int64 
createIteamPG connString iteam = fromSqlKey <$> runAction connString (insert iteam)

createTopicPG :: PGInfo -> Topic  -> IO Int64
createTopicPG connString topic = fromSqlKey <$> runAction connString (insert topic)


deleteIteamPG :: PGInfo -> Int64 -> IO () 
deleteIteamPG connString bid = runAction connString (delete iteamKey)
  where
    iteamKey :: Key Iteam
    iteamKey = toSqlKey bid
