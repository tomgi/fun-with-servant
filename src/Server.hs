{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server (API, api, server) where

import qualified Client
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Char8 (pack)
import           Servant
import           Types

type API = "users" :> Get '[JSON] [User]
      :<|> "time"  :> Get '[JSON] DateTimeAndIP

api :: Proxy API
api = Proxy

server :: Server API
server = return users
    :<|> currentDateTimeHandler

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

currentDateTimeHandler :: Handler DateTimeAndIP
currentDateTimeHandler = do
  r1 <- liftIO Client.currentDateTime
  r2 <- liftIO Client.currentIP
  let res = (,) <$> r1 <*> r2
  case res of
    Left err -> throwError myerr
      where myerr :: ServantErr
            myerr = err503 { errBody = pack $ "Something went wrong\n" ++ err }
    Right (dateTime, ip) -> return $ DateTimeAndIP dateTime ip
