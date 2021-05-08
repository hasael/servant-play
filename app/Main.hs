{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppConfig
import Control.Concurrent
import Control.Concurrent.Async
import Data.Yaml
import Domain.DbRepository
import Lib
import Impl.RealDb
import Impl.InMemoryDb
import Domain.AppState
import System.Environment

main :: IO ()
main = do 
  args <- getArgs
  print $ "Args: " ++ show args
  state <- newState
  let initArg = if not $ null args then Just $ head args else Nothing 
  print initArg
  case initArg of
    Just "mem" -> startInMemory state 
    _ -> startReal "./local-config.yaml" state

startInMemory :: AppState -> IO ()
startInMemory state = do
  db <- newDB 
  let env = InMemEnv state db
  startAligner env
  concurrently_ (startApp 8080 $ app env) (scheduled env 4000000)


startReal :: FilePath -> AppState -> IO ()
startReal config state = do
  config <- readConfig config
  connectionsPool <- initConnection $ (connectionString . db) config
  initDb connectionsPool
  let env = Env state connectionsPool
  startAligner env
  concurrently_ (startApp 8080 $ app env) (scheduledR env 4000000)


scheduled :: InMemEnv -> Int -> IO ()
scheduled env delay = threadDelay delay >> merge_ env >> scheduled env delay

scheduledR :: Env -> Int -> IO ()
scheduledR env delay = threadDelay delay >> merge_ env >> scheduledR env delay

readConfig :: FilePath -> IO AppConfig
readConfig filepath = do 
  dbConfig <- lookupEnv "DB_CONFIG"
  print dbConfig
  case dbConfig of
    Just config -> return $ AppConfig $ DbConfig config
    _ -> decodeFileThrow filepath