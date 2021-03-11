{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module RealDb where

import Data.Pool
import Database.PostgreSQL.Simple
import DbRepository
import qualified TransactionRepository as T
import qualified UserRepository as U
import Data.ByteString.Char8

initDb :: Pool Connection -> IO ()
initDb connectionsPool = withResource connectionsPool $ \conn -> do
                         execute_ conn "CREATE TABLE IF NOT EXISTS users (id bigserial not null, name text not null, last_name text not null, amount float8 not null)" 
                         execute_ conn "CREATE TABLE IF NOT EXISTS transactions (id bigserial not null, user_id int8 not null, amount float8 not null, transaction_type text not null )" 
                         return ()

initConnection :: String -> IO (Pool Connection)
initConnection connStr = createPool (connectPostgreSQL $ pack connStr) 
                                    close
                                    2 -- stripes
                                    60 --seconds to keep alive if unused
                                    10 --max 20 connection per stripe

instance DbRepository IO (Pool Connection) where

    getUserAmount pool userId = withResource pool $ U.getUserAmount userId

    updateUserAmount pool userId amount = withResource pool $ U.updateUserAmount userId amount

    getAllUsers pool = withResource pool U.getAllUsers 

    getUserById pool userId = withResource pool $ U.getUserById userId

    insertUser pool u = withResource pool $ U.insertUser u

    getTransactionById pool trxId = withResource pool $ T.getTransactionById trxId

    getTransactions pool userId = withResource pool $ T.getTransactions userId

    getAllTransactions pool = withResource pool T.getAllTransactions 

    insertCreditTransaction pool userId amount = withResource pool $ T.insertCreditTransaction userId amount 

    insertDebitTransaction pool userId amount = withResource pool $ T.insertDebitTransaction userId amount  
