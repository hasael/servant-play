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

main :: IO ()
main = do
  config <- readConfig
  connectionsPool <- initConnection $ (connectionString . db) config
  startAligner connectionsPool
  initDb connectionsPool
  concurrently_ (startApp 8080 $ app connectionsPool) (scheduled connectionsPool 3000000)

scheduled :: (DbRepository IO a) => a -> Int -> IO ()
scheduled conn delay = threadDelay delay >> merge_ conn >> scheduled conn delay

readConfig :: IO AppConfig
readConfig = decodeFileThrow "./local-config.yaml"
