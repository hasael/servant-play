{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import AppConfig
import Data.Yaml
import RealDb

main :: IO ()
main = do 
     config <- readConfig
     print config
     connectionsPool <- initConnection $ (connectionString . db) config
     initDb connectionsPool
     startApp connectionsPool

readConfig :: IO AppConfig
readConfig = decodeFileThrow "./local-config.yaml" 


