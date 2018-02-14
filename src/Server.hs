{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server (API, api, server) where

import           Config
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Int
import           Database.Persist.Sql
import qualified Db
import           GHC.Int
import qualified RestClient
import           Schema
import           Servant
import           Types

type API = "users" :> Get '[JSON] [User]
      :<|> "users" :> "create"
                   :> ReqBody '[JSON] User
                   :> Post '[JSON] Int64
      :<|> "time"  :> Get '[JSON] DateTimeAndIP

api :: Proxy API
api = Proxy

server :: Reader Config (Server API)
server = do
    config <- ask
    return $ users (pool config)
      :<|> createUser (pool config)
      :<|> currentDateTimeHandler

users :: ConnectionPool -> Handler [User]
users pool = do
  u <- liftIO $ Db.allUsers pool
  return $ map entityVal u

createUser :: ConnectionPool -> User -> Handler Int64
createUser pool user = do
  userId <- liftIO $ Db.storeUser user pool
  return $ fromSqlKey userId


currentDateTimeHandler :: Handler DateTimeAndIP
currentDateTimeHandler = do
  r1 <- liftIO RestClient.currentDateTime
  r2 <- liftIO RestClient.currentIP
  let res = (,) <$> r1 <*> r2
  case res of
    Left err -> throwError myerr
      where myerr :: ServantErr
            myerr = err503 { errBody = pack $ "Something went wrong\n" ++ err }
    Right (dateTime, ip) -> return $ DateTimeAndIP dateTime ip
