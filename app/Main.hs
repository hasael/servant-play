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
import InMemoryDb
import Models
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print $ "Args: " ++ show args
  state <- newState
  let initArg = if not $ null args then Just $ head args else Nothing 
  print initArg
  case initArg of
    Just "local" -> startReal "./local-config.yaml" state
    Just "dev" -> startReal "./dev-config.yaml" state
    _ -> startInMemory state

startInMemory :: AppState -> IO ()
startInMemory state = do
  db <- newDB 
  startAligner db state
  concurrently_ (startApp 8080 $ app db state) (scheduled db state 4000000)


startReal :: FilePath -> AppState -> IO ()
startReal config state = do
  config <- readConfig config
  connectionsPool <- initConnection $ (connectionString . db) config
  initDb connectionsPool
  startAligner connectionsPool state
  concurrently_ (startApp 8080 $ app connectionsPool state) (scheduled connectionsPool state 4000000)

scheduled :: (DbRepository IO a) => a -> AppState -> Int -> IO ()
scheduled conn state delay = threadDelay delay >> merge_ conn state >> scheduled conn state delay

readConfig :: FilePath -> IO AppConfig
readConfig = decodeFileThrow
