{-# LANGUAGE OverloadedStrings #-}

module TransactionRepository where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.FromRow ()
import GHC.Int ( Int64 )
import Models ( Transaction, TransactionType(..) ) 

instance FromField TransactionType where
  fromField f (Just "Debit") = return Debit
  fromField f _ = return Credit

instance FromRow Transaction

getTransactionById :: Connection -> Int -> IO (Maybe Transaction)
getTransactionById conn transactionId = do
    rows <- query conn "SELECT id, user_id, amount, transaction_type  from transactions where id = ?" (Only transactionId) 
    case rows of
        [] -> return Nothing
        (x:_) -> return $ Just x

getTransactions :: Connection -> Int -> IO [Transaction]
getTransactions conn userId = query conn "SELECT id, user_id, amount, transaction_type  from transactions where user_id = ?" (Only userId) 

insertCreditTransaction :: Connection -> Int -> Double -> IO Int64
insertCreditTransaction conn userId amount = execute conn "INSERT INTO transactions(id, user_id, amount, transaction_type) VALUES (default,?,?,'Credit')" (userId, amount) 

insertDebitTransaction :: Connection -> Int -> Double -> IO Int64
insertDebitTransaction conn userId amount = execute conn "INSERT INTO transactions(id, user_id, amount, transaction_type) VALUES (default,?,?,'Debit')" (userId, amount) 