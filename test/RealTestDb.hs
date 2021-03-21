{-# LANGUAGE OverloadedStrings #-}

module RealTestDb where

import Data.Pool
import Database.PostgreSQL.Simple
import RealDb
import Test.QuickCheck
import Test.QuickCheck.Monadic
import TestBase

instance CanPropertyTest IO where
  toProperty = ioProperty

monadicPropIO :: PropertyM IO () -> Property
monadicPropIO = monadic toProperty

initTestDbConnection :: String -> IO (Pool Connection)
initTestDbConnection = initConnection

cleanTables :: Pool Connection -> IO ()
cleanTables connectionsPool = withResource connectionsPool $ \conn -> do
  execute_ conn "DELETE FROM users"
  execute_ conn "DELETE FROM transactions"
  return ()
