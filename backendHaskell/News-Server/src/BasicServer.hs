{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module BasicServer where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
--import           Network.Wai.Middleware.Cors  --NISAM USPJELA DA PODESIM DA RADI OVO, TAKO DA SE MORA KLIJENT OTVARATI U BROWSERU SA ISKLJUCENIM SIGURNOSNIM FLAG-OVIMA
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database (fetchIteamPG, fetchAllIteamsPG, fetchAllIteamsForTopicPG, fetchRecentIteamsPG, fetchPrekoJoinaPG, createTopicPG, createIteamPG, deleteIteamPG, localConnString)
import           BasicSchema


type IteamsAPI = 
       "iteams" :> Capture "iteamid" Int64 :> Get '[JSON] Iteam
  :<|> "iteams" :> ReqBody '[JSON] Iteam :> Post '[JSON] Int64
  :<|> "iteams" :> "sve" :> Get '[JSON] [Entity Iteam]
  :<|> "iteams" :> "enviroment" :> Get '[JSON] [Entity Iteam]
  :<|> "iteams" :> "sport" :> Get '[JSON] [Entity Iteam]
  :<|> "iteams" :> "technology" :> Get '[JSON] [Entity Iteam]
  :<|> "iteams" :> "health" :> Get '[JSON] [Entity Iteam]
  :<|> "iteams" :> "world" :> Get '[JSON] [Entity Iteam]
  :<|> "iteams" :> "join" :> Get '[JSON] [Entity Iteam]
  :<|> "iteams" :> Capture "iteamid" Int64 :> Post '[JSON] () --Delete ZASAD
--  :<|> "Topics" :> ReqBody '[JSON] Iteam :> Post '[JSON] Int64  --insert jezik, nesto ne radi


iteamsAPI :: Proxy IteamsAPI
iteamsAPI = Proxy :: Proxy IteamsAPI

fetchIteamsHandler :: ConnectionString -> Int64 -> Handler Iteam
fetchIteamsHandler connString bid = do
  maybeIteam <- liftIO $ fetchIteamPG connString bid
  case maybeIteam of
    Just iteam -> return iteam
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find Iteam with that ID" })


fetchAllIteamsElmHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsElmHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 1

fetchAllIteamsHaskellHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsHaskellHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 2

fetchAllIteamsElixirHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsElixirHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 3

fetchAllIteamsFsharpHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsFsharpHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 4

fetchAllIteamsClojureHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsClojureHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 5

fetchAllIteamsOcamlHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsOcamlHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 6

fetchAllIteamsScalaHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsScalaHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 7

fetchAllIteamsRacketHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsRacketHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 8

fetchAllIteamsSchemeHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsSchemeHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 9

fetchAllIteamsLispHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsLispHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 10

fetchAllIteamsMlHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsMlHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 11

fetchAllIteamsAplHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsAplHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 12

fetchAllIteamsMirandaHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsMirandaHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 13

fetchAllIteamsAgdaHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsAgdaHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 14

fetchAllIteamsErlangHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsErlangHandler connString = liftIO $ fetchAllIteamsForTopicPG connString 15

fetchAllIteamsHandler :: ConnectionString -> Handler [Entity Iteam]
fetchAllIteamsHandler connString = liftIO $ fetchAllIteamsPG connString

createIteamHandler :: ConnectionString -> Iteam -> Handler Int64
createIteamHandler connString iteam = liftIO $ createIteamPG connString iteam

createTopicHandler :: ConnectionString -> Topic -> Handler Int64
createTopicHandler connString topic = liftIO $ createTopicPG connString topic

fetchRecentIteamsHandler :: ConnectionString -> Handler [(Entity Topic , Entity Iteam)]
fetchRecentIteamsHandler connString = liftIO $ fetchRecentIteamsPG localConnString 

fetchPrekoJoinaHandler :: ConnectionString -> Handler [Entity Iteam]
fetchPrekoJoinaHandler connString = liftIO $ fetchPrekoJoinaPG localConnString

deleteIteamHandler :: ConnectionString -> Int64 -> Handler ()
deleteIteamHandler connString bid = liftIO $ deleteIteamPG connString bid

iteamsServer :: ConnectionString -> Server IteamsAPI
iteamsServer connString = 
  (fetchIteamsHandler connString) :<|> 
  (createIteamHandler connString) :<|>
  (fetchAllIteamsHandler connString) :<|>
  (fetchAllIteamsEnviromentHandler connString) :<|>
  (fetchAllIteamsHealthHandler connString) :<|>
  (fetchAllIteamsSportHandler connString) :<|>
  (fetchAllIteamsWorldHandler connString) :<|>
  (fetchAllIteamsTechnologyHandler connString) :<|>
  (fetchAllIteamsAllNewsHandler connString) :<|>
  (fetchRecentIteamsHandler connString) :<|>
  (fetchPrekoJoinaHandler connString) :<|>
  (deleteIteamHandler connString)
--  (createTopicHandler connString)

runServer :: IO ()
runServer = run 5000 (serve iteamsAPI (iteamsServer localConnString))

