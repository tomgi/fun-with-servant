module Config where

import           Database.Persist.Sql

newtype Config = Config {
  pool :: ConnectionPool
}

