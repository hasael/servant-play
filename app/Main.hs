{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppConfig
import Control.Concurrent
import Control.Concurrent.Async
import Data.Yaml
import DbRepository
import Lib
import RealDb
import Models

main :: IO ()
main = do
  config <- readConfig
  connectionsPool <- initConnection $ (connectionString . db) config
  state <- newState
  initDb connectionsPool
  startAligner connectionsPool state
  concurrently_ (startApp 8080 $ app connectionsPool state) (scheduled connectionsPool state 3000000)

scheduled :: (DbRepository IO a) => a -> AppState -> Int -> IO ()
scheduled conn state delay = threadDelay delay >> merge_ conn state >> scheduled conn state delay

readConfig :: IO AppConfig
readConfig = decodeFileThrow "./local-config.yaml"
