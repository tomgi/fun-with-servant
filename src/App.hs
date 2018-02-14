{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App (startApp) where

import           Config
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString.Char8    (pack)
import           Data.Maybe
import           Db
import qualified Docs
import           Network.Wai.Handler.Warp
import           Servant
import qualified Server
import           System.Environment
import           Text.Read

type DocsAPI = Server.API :<|> Raw

api :: Proxy DocsAPI
api = Proxy

server :: Reader Config (Server DocsAPI)
server = do
  s <- Server.server
  return (s :<|> Docs.server)

app :: Reader Config Application
app = server >>= \s -> return $ serve api s

startApp :: IO ()
startApp = do
  dbUrlEnv <- lookupEnv "DATABASE_URL"
  let dbEngine = case dbUrlEnv of
        Nothing         -> Sqlite
        Just connString -> Postgres $ pack connString

  pool <- makePool dbEngine
  putStrLn "Migrating database"
  Db.runMigrations pool

  portEnv <- lookupEnv "PORT"
  let port = fromMaybe 8080 (portEnv >>= readMaybe)
  putStrLn $ "Listening on " ++ show port

  run port $ runReader app (Config pool)
