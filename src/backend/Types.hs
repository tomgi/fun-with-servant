{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson
import           GHC.Generics

data DateTime = DateTime
  { time                     :: String
  , date                     :: String
  , milliseconds_since_epoch :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON DateTime
instance ToJSON DateTime

newtype IP = IP
  { ip                     :: String
  } deriving (Eq, Show, Generic)

instance FromJSON IP
instance ToJSON IP

data DateTimeAndIP = DateTimeAndIP
  { dateTime  :: DateTime
  , ipAddress :: IP
  } deriving (Eq, Show, Generic)

instance ToJSON DateTimeAndIP
