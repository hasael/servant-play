{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import AppConfig
import Database.PostgreSQL.Simple
import Data.Pool
import Data.Yaml
import Data.ByteString.Char8

main :: IO ()
main = do 
     config <- readConfig
     print config
     connectionsPool <- initConnection $ (connectionString . db) config
     initDb connectionsPool
     startApp connectionsPool

initConnection :: String -> IO (Pool Connection)
initConnection connStr = createPool (connectPostgreSQL $ pack connStr) 
                                    close
                                    2 -- stripes
                                    60 --seconds to keep alive if unused
                                    10 --max 20 connection per stripe

readConfig :: IO AppConfig
readConfig = decodeFileThrow "./local-config.yaml" 

initDb :: Pool Connection -> IO ()
initDb connectionsPool = withResource connectionsPool $ \conn -> do
                         execute_ conn "CREATE TABLE IF NOT EXISTS users (id bigserial not null, name text not null, last_name text not null, amount float8 not null)" 
                         execute_ conn "CREATE TABLE IF NOT EXISTS transactions (id bigserial not null, user_id int8 not null, amount float8 not null, transaction_type text not null )" 
                         return ()