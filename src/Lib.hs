{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib (startApp) where

import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Maybe
import qualified Docs
import           Network.Wai.Handler.Warp
import           Servant
import qualified Server
import           System.Environment
import           Text.Read

type DocsAPI = Server.API :<|> Raw

api :: Proxy DocsAPI
api = Proxy

server :: Server DocsAPI
server = Server.server :<|> Docs.server

startApp :: IO ()
startApp = do
  port <- lookupEnv "PORT"
  let portNumber = fromMaybe 8080 (port >>= readMaybe)
  putStrLn $ "Listening on " ++ show portNumber
  run portNumber app

app :: Application
app = serve api server
