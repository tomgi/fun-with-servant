module RestClient (currentDateTime, currentIP) where

import           Control.Monad
import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteString.Lazy.Char8
import           Network.HTTP
import           Network.HTTP.Stream        (ConnError)
import           Types

type Error = String

currentDateTime :: IO (Either Error DateTime)
currentDateTime = getAndParse "http://time.jsontest.com"

currentIP :: IO (Either Error IP)
currentIP = getAndParse "http://ip.jsontest.com"

getAndParse :: FromJSON a => String -> IO (Either Error a)
getAndParse url = do
  rsp <- simpleHTTP $ getRequest url
  let body = first show $ rspBody <$> rsp
  let parsed = eitherDecode . pack <$> body
  return $ join parsed
