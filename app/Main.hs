{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib
import AppConfig
import Data.Yaml
import RealDb
import Control.Concurrent.Async
import Control.Concurrent
import DbRepository

main :: IO ()
main = do 
     config <- readConfig
     print config
     connectionsPool <- initConnection $ (connectionString . db) config
     startAligner connectionsPool
     initDb connectionsPool
     concurrently_ (startApp 8080 $ app connectionsPool ) (scheduled connectionsPool 3000000)
     
scheduled :: (DbRepository IO a)  => a -> Int -> IO ()
scheduled conn delay = threadDelay delay >> merge_ conn >> scheduled conn delay

readConfig :: IO AppConfig
readConfig = decodeFileThrow "./local-config.yaml" 

