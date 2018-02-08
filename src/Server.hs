{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server (startApp) where

import qualified Client
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Maybe
import           Network.Wai.Handler.Warp
import           Servant
import           System.Environment
import           Text.Read
import           Types

type API = "users" :> Get '[JSON] [User]
      :<|> "time"  :> Get '[JSON] DateTimeAndIP

startApp :: IO ()
startApp = do
  port <- lookupEnv "PORT"
  let portNumber = fromMaybe 8080 (port >>= readMaybe)
  putStrLn $ "Listening on " ++ show portNumber
  run portNumber app

app :: Application
app = serve api server

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
