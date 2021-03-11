{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib
import AppConfig
import Data.Yaml
import RealDb
import Control.Concurrent.Async
import Control.Concurrent

import Instances
import DbRepository
import Models
import GCounter

--main :: IO ()
--main = concurrently_ server scheduled

main :: IO ()
main = do 
     config <- readConfig
     print config
     connectionsPool <- initConnection $ (connectionString . db) config
     startCrdt connectionsPool
     initDb connectionsPool
     concurrently_ (startApp connectionsPool) (scheduled connectionsPool)
     

readConfig :: IO AppConfig
readConfig = decodeFileThrow "./local-config.yaml" 

scheduled :: (DbRepository IO a, GCounter Transaction Int)  => a -> IO ()
scheduled conn = threadDelay 3000000 >> scheduledJob conn >> scheduled conn

startCrdt :: (DbRepository IO a, GCounter Transaction Int)  => a -> IO ()
startCrdt conn = do
     trxs <- getAllTransactions conn
     sequence $ fmap (\t -> increment (userId t) t ) trxs
     return ()

