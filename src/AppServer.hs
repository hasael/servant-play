{-# LANGUAGE FlexibleContexts #-}

module AppServer
  ( app,
  )
where

import AppAPI
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import DbRepository
import GCounter
import Instances
import Models
import Network.Wai
import Servant
import TransactionService

api :: Proxy API
api = Proxy

app :: (DbRepository IO a) => a -> Application
app connectionsPool = serve api $ server connectionsPool

userServer :: (DbRepository IO a) => a -> Server UserAPI
userServer connectionsPool =
  fetchUsers connectionsPool
    :<|> fetchUser connectionsPool
    :<|> createUser connectionsPool

transactionsServer :: (DbRepository IO a) => a -> Server TransactionsAPI
transactionsServer connectionsPool =
  fetchTransactions connectionsPool
    :<|> addCreditTransaction connectionsPool
    :<|> addDebitTransaction connectionsPool

server :: (DbRepository IO a) => a -> Server API
server connectionsPool =
  userServer connectionsPool
    :<|> transactionsServer connectionsPool

fetchUsers :: DbRepository IO a => a -> Handler [User]
fetchUsers conn = liftIO $ getAllUsers conn

fetchUser :: DbRepository IO a => a -> UserId -> Handler User
fetchUser conn userId = liftIO (getUserById conn userId) >>= notFoundResponse

createUser :: DbRepository IO a => a -> User -> Handler User
createUser conn user = do
  mu <- liftIO $ insertUser conn user
  case mu of
    Just u -> return u
    Nothing -> throwError err404

fetchTransactions :: DbRepository IO a => a -> UserId -> Handler [Transaction]
fetchTransactions conn userId = do
  users <- liftIO $ getTransactions conn userId
  case users of
    [] -> throwError err404
    a -> return a

addCreditTransaction :: (DbRepository IO a) => a -> UserId -> Amount  -> Handler Transaction
addCreditTransaction conn userId amount = do
  result <- liftIO $ createCreditTransaction conn userId amount
  case result of
    CreditUserNotFound -> throwError err404
    CorrectCredit t -> liftIO $ forkIO (void (increment userId $ trxAmount t)) >> return t

addDebitTransaction :: (DbRepository IO a) => a -> UserId -> Amount  -> Handler Transaction
addDebitTransaction conn userId amount = do
  result <- liftIO $ createDebitTransaction conn userId amount
  case result of
    DebitUserNotFound -> throwError err404
    IncorrectAmount -> throwError err403
    CorrectDebit t -> liftIO $ forkIO (void (increment userId $ trxAmount t)) >> return t

notFoundResponse :: Maybe a -> Handler a
notFoundResponse Nothing = throwError err404
notFoundResponse (Just r) = return r
