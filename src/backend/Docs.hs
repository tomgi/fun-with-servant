{-# LANGUAGE OverloadedStrings #-}

module Docs (server) where

import           Data.Default            (def)
import           Data.Text.Lazy          (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           GHC.Int
import           Network.HTTP.Types
import           Network.Wai
import           Schema
import           Servant.API
import           Servant.Docs
import           Servant.Docs.Pandoc
import           Servant.Server
import qualified Server
import           Text.Pandoc
import           Types

instance ToSample User where
  toSamples _ = singleSample $ User "Isaac" "Newton"

instance ToSample DateTimeAndIP where
  toSamples _ = singleSample $ DateTimeAndIP (DateTime "02:42:35 PM" "02-09-2018" 1518187355975) (IP "127.0.0.1")

instance ToSample Int64 where
  toSamples _ = singleSample 1

apiDocs :: API
apiDocs = docs Server.api

server :: Server Raw
server = Tagged serveDocs where
    serveDocs _ respond =
        respond $ responseLBS ok200 [html] $ (encodeUtf8 . pack) $ writeHtmlString def (pandoc apiDocs)
    html = ("Content-Type", "text/html")
