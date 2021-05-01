{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Server
  ( server,
    api,
  )
where

import Api.AppAPI
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Error.Class
import Control.Monad.Reader
import Domain.DbRepository
import Domain.GCounter
import Impl.GCounter
import Domain.Transaction
import Domain.User
import Domain.Helper
import Domain.AppState
import Network.Wai
import Servant
import Domain.TransactionService
  ( CreditOpResult (CorrectCredit, CreditUserNotFound),
    DebitOpResult (CorrectDebit, DebitUserNotFound, IncorrectAmount),
    createCreditTransaction,
    createDebitTransaction,
  )

userServer :: (DbRepository m env, MonadReader env m, MonadIO m, MonadError ServerError m) => ServerT UserAPI m
userServer =
  fetchUsers
    :<|> fetchUser
    :<|> createUser

transactionsServer :: (DbRepository m env, MonadReader env m, MonadIO m, HasAppState env, MonadError ServerError m) => ServerT TransactionsAPI m
transactionsServer =
  fetchTransactions
    :<|> addCreditTransaction
    :<|> addDebitTransaction

server :: (DbRepository m env, MonadReader env m, MonadIO m, HasAppState env, MonadError ServerError m) => ServerT API m
server =
  userServer
    :<|> transactionsServer
    :<|> getVersion

fetchUsers :: (DbRepository m env, MonadReader env m) => m [User]
fetchUsers = do
  env <- ask
  getAllUsers env

fetchUser :: (DbRepository m env, MonadReader env m, MonadError ServerError m) => UserId -> m User
fetchUser userId = do
  env <- ask
  getUserById env userId >>= notFoundResponse

createUser :: (DbRepository m env, MonadReader env m, MonadError ServerError m) => User -> m User
createUser user = do
  env <- ask
  mu <- insertUser env user
  case mu of
    Just u -> return u
    Nothing -> throwError err404

fetchTransactions :: (DbRepository m env, MonadReader env m, MonadError ServerError m) => UserId -> m [Transaction]
fetchTransactions userId = do
  env <- ask
  users <- getTransactions env userId
  case users of
    [] -> throwError err404
    a -> return a

addCreditTransaction :: (DbRepository m env, MonadReader env m, MonadIO m, HasAppState env, MonadError ServerError m) => UserId -> Amount -> m Transaction
addCreditTransaction userId amount = do
  env <- ask
  result <- createCreditTransaction env userId amount
  let state = getAppState env
  case result of
    CreditUserNotFound -> throwError err404
    CorrectCredit t -> liftIO $ forkIO (void (increment state userId $ trxAmount t)) >> return t

addDebitTransaction :: (DbRepository m env, MonadReader env m, HasAppState env, MonadIO m, MonadError ServerError m) => UserId -> Amount -> m Transaction
addDebitTransaction userId amount = do
  env <- ask
  result <- createDebitTransaction env userId amount
  let state = getAppState env
  case result of
    DebitUserNotFound -> throwError err404
    IncorrectAmount -> throwError err403
    CorrectDebit t -> liftIO $ forkIO (void (increment state userId $ trxAmount t)) >> return t

getVersion :: Monad m => m String
getVersion = return "0.1.0.4"

notFoundResponse :: (Monad m, MonadError ServerError m) => Maybe a -> m a
notFoundResponse Nothing = throwError err404
notFoundResponse (Just r) = return r
