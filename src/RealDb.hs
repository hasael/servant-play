{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module RealDb where

import Control.Monad (void)
import Data.ByteString.Char8
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import DbRepository
import Models
import Control.Monad.IO.Class ( MonadIO(liftIO) )

initDb :: Pool Connection -> IO ()
initDb connectionsPool = withResource connectionsPool $ \conn -> do
  execute_ conn "CREATE TABLE IF NOT EXISTS users (id bigserial not null, name text not null, last_name text not null, amount float8 not null)"
  execute_ conn "CREATE TABLE IF NOT EXISTS transactions (id bigserial not null, user_id int8 not null, amount float8 not null, transaction_type text not null )"
  return ()

initConnection :: String -> IO (Pool Connection)
initConnection connStr =
  createPool
    (connectPostgreSQL $ pack connStr)
    close
    2 -- stripes
    60 --seconds to keep alive if unused
    10 --max 20 connection per stripe

instance FromRow User

instance FromField TransactionType where
  fromField f (Just "Debit") = return Debit
  fromField f _ = return Credit

instance FromRow Transaction

instance DbRepository IO (Pool Connection) where
  getUserAmount pool userId = withResource pool $ \conn -> do
    amount <- query conn "SELECT amount from users where id =?" (Only userId)
    case amount of
      [] -> return Nothing
      ((Only a) : xs) -> return $ Just a

  updateUserAmount pool userId amount = withResource pool $ \conn ->
    void (execute conn "UPDATE users set amount= ? where id=?" (amount, userId))

  getAllUsers pool = withResource pool $ \conn ->
    query_ conn "SELECT id, name, last_name, amount from users"

  getUserById pool userId = withResource pool $ \conn -> do
    rows <- query conn "SELECT id, name, last_name, amount from users where id = ?" (Only userId)
    case rows of
      [] -> return Nothing
      (x : _) -> return $ Just x

  insertUser pool user = withResource pool $ \conn -> do
    rows <- query conn "INSERT INTO users VALUES (default,?,?,0) RETURNING *" [name user, lastName user]
    case rows of
      [] -> return Nothing
      (x : _) -> return $ Just x

  getTransactionById pool transactionId = withResource pool $ \conn -> do
    rows <- query conn "SELECT id, user_id, amount, transaction_type  from transactions where id = ?" (Only transactionId)
    case rows of
      [] -> return Nothing
      (x : _) -> return $ Just x

  getTransactions pool userId = withResource pool $ \conn ->
    query conn "SELECT id, user_id, amount, transaction_type  from transactions where user_id = ?" (Only userId)

  getAllTransactions pool = withResource pool $ \conn ->
    query conn "SELECT id, user_id, amount, transaction_type  from transactions" ()

  insertCreditTransaction pool userId amount = withResource pool $ \conn -> do
    rows <- query conn "INSERT INTO transactions(id, user_id, amount, transaction_type) VALUES (default,?,?,'Credit') RETURNING *" (userId, amount)
    case rows of
      [] -> return Nothing
      (x : _) -> return $ Just x

  insertDebitTransaction pool userId amount = withResource pool $ \conn -> do
    rows <- query conn "INSERT INTO transactions(id, user_id, amount, transaction_type) VALUES (default,?,?,'Debit') RETURNING *" (userId, amount)
    case rows of
      [] -> return Nothing
      (x : _) -> return $ Just x

data Env = Env {
  crdtState :: AppState,
  connectionsPool :: Pool Connection
}

instance HasAppState Env where
  getAppState = crdtState

instance (DbRepository IO (Pool Connection)) => DbRepository (MyHandler Env) Env where
  getUserAmount env userId = liftIO $ getUserAmount (connectionsPool env) userId

  updateUserAmount env userId amount = liftIO $ updateUserAmount (connectionsPool env) userId amount

  getAllUsers env = liftIO $ getAllUsers (connectionsPool env)

  getUserById env userId = liftIO $ getUserById (connectionsPool env) userId

  insertUser env user = liftIO $ insertUser (connectionsPool env) user

  getTransactionById env transactionId = liftIO $ getTransactionById (connectionsPool env) transactionId

  getTransactions env userId = liftIO $ getTransactions (connectionsPool env) userId

  getAllTransactions env = liftIO $ getAllTransactions (connectionsPool env)

  insertCreditTransaction env userId amount = liftIO $ insertCreditTransaction (connectionsPool env) userId amount

  insertDebitTransaction env userId amount = liftIO $ insertDebitTransaction (connectionsPool env) userId amount

instance (DbRepository IO (Pool Connection)) => DbRepository (EnvHandler Env) Env where
  getUserAmount env userId = liftIO $ getUserAmount (connectionsPool env) userId

  updateUserAmount env userId amount = liftIO $ updateUserAmount (connectionsPool env) userId amount

  getAllUsers env = liftIO $ getAllUsers (connectionsPool env)

  getUserById env userId = liftIO $ getUserById (connectionsPool env) userId

  insertUser env user = liftIO $ insertUser (connectionsPool env) user

  getTransactionById env transactionId = liftIO $ getTransactionById (connectionsPool env) transactionId

  getTransactions env userId = liftIO $ getTransactions (connectionsPool env) userId

  getAllTransactions env = liftIO $ getAllTransactions (connectionsPool env)

  insertCreditTransaction env userId amount = liftIO $ insertCreditTransaction (connectionsPool env) userId amount

  insertDebitTransaction env userId amount = liftIO $ insertDebitTransaction (connectionsPool env) userId amount

instance (DbRepository IO (Pool Connection)) => DbRepository IO Env where
  getUserAmount env userId = getUserAmount (connectionsPool env) userId

  updateUserAmount env userId amount = updateUserAmount (connectionsPool env) userId amount

  getAllUsers env = getAllUsers (connectionsPool env)

  getUserById env userId = getUserById (connectionsPool env) userId

  insertUser env user = insertUser (connectionsPool env) user

  getTransactionById env transactionId = getTransactionById (connectionsPool env) transactionId

  getTransactions env userId = getTransactions (connectionsPool env) userId

  getAllTransactions env = getAllTransactions (connectionsPool env)

  insertCreditTransaction env userId amount = insertCreditTransaction (connectionsPool env) userId amount

  insertDebitTransaction env userId amount = insertDebitTransaction (connectionsPool env) userId amount