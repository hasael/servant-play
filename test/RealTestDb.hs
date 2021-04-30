{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module RealTestDb where

import Data.Pool
import Database.PostgreSQL.Simple
import Impl.RealDb
import Test.QuickCheck
import Test.QuickCheck.Monadic
import TestBase
import Domain.Models
import Control.Monad.Reader  

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
