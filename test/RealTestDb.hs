{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts           #-} 

module RealTestDb where

import RealDb
import Data.Pool
import Database.PostgreSQL.Simple
import TestBase
import Test.QuickCheck
import Test.QuickCheck.Monadic

instance CanPropertyTest IO where
    toProperty = ioProperty 


monadicPropIO :: (CanPropertyTest IO) => PropertyM IO () -> Property
monadicPropIO = monadic toProperty 

initTestDbConnection = initConnection

cleanTables :: Pool Connection -> IO ()
cleanTables connectionsPool = withResource connectionsPool $ \conn -> do
                         execute_ conn "DELETE FROM users" 
                         execute_ conn "DELETE FROM transactions" 
                         return ()