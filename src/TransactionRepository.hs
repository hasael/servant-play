{-# LANGUAGE OverloadedStrings #-}

module TransactionRepository where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.FromRow ()
import Models ( Transaction, TransactionType(..) ) 

instance FromField TransactionType where
  fromField f (Just "Debit") = return Debit
  fromField f _ = return Credit

instance FromRow Transaction

getTransactionById ::  Int -> Connection -> IO (Maybe Transaction)
getTransactionById transactionId conn = do
    rows <- query conn "SELECT id, user_id, amount, transaction_type  from transactions where id = ?" (Only transactionId) 
    case rows of
        [] -> return Nothing
        (x:_) -> return $ Just x

getTransactions ::  Int -> Connection -> IO [Transaction]
getTransactions userId conn = query conn "SELECT id, user_id, amount, transaction_type  from transactions where user_id = ?" (Only userId) 

getAllTransactions :: Connection -> IO [Transaction]
getAllTransactions conn = query conn "SELECT id, user_id, amount, transaction_type  from transactions" () 

insertCreditTransaction ::  Int -> Double -> Connection -> IO (Maybe Transaction) 
insertCreditTransaction userId amount conn = do
    rows <- query conn "INSERT INTO transactions(id, user_id, amount, transaction_type) VALUES (default,?,?,'Credit') RETURNING *" (userId, amount) 
    case rows of
        [] -> return Nothing
        (x:_) -> return $ Just x

insertDebitTransaction ::  Int -> Double -> Connection -> IO (Maybe Transaction)
insertDebitTransaction userId amount conn = do
    rows <- query conn "INSERT INTO transactions(id, user_id, amount, transaction_type) VALUES (default,?,?,'Debit') RETURNING *" (userId, amount) 
    case rows of
        [] -> return Nothing
        (x:_) -> return $ Just x