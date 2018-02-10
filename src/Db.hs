{-# LANGUAGE OverloadedStrings #-}

module Db where

import           Control.Monad.Logger
import           Data.ByteString.Internal
import           Database.Persist.Postgresql
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Schema

data DatabaseEngine = Sqlite | Postgres ByteString

makePool :: DatabaseEngine -> IO ConnectionPool
makePool Sqlite = runStdoutLoggingT $ createSqlitePool (sqlDatabase $ SqliteConf "./fun-with-servant.db" 1) 1
makePool (Postgres connStr) = runStdoutLoggingT $ createPostgresqlPool connStr 8

runMigrations :: ConnectionPool -> IO ()
runMigrations = runSqlPool $ runMigration migrateAll

allUsers :: ConnectionPool -> IO [Entity User]
allUsers = runSqlPool $ selectList [] []

storeUser :: User -> ConnectionPool -> IO (Key User)
storeUser user = runSqlPool $ insert user
