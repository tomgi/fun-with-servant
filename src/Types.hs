{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson
import           GHC.Generics

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User

data DateTime = DateTime
  { time                     :: String
  , date                     :: String
  , milliseconds_since_epoch :: Integer
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
