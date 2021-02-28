{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import LocalConfig
import Database.PostgreSQL.Simple
import Data.Pool
import Data.Yaml
import Data.ByteString.Char8

main :: IO ()
main = do 
     config <- readConfig
     print config
     connectionsPool <- initConnection $ (connectionString . db) config
     startApp connectionsPool

initConnection :: String -> IO (Pool Connection)
initConnection connStr = createPool (connectPostgreSQL $ pack connStr) 
                                    close
                                    2 -- stripes
                                    60 --seconds to keep alive if unused
                                    10 --max 20 connection per stripe

readConfig :: IO AppConfig
readConfig = decodeFileThrow "./local-config.yaml" 