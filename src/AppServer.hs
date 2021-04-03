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
import Control.Monad.Trans.Reader


type AppHandler = ReaderT AppState Handler

api :: Proxy API
api = Proxy

nt :: AppState -> AppHandler a -> Handler a
nt s x = runReaderT x s

app :: (DbRepository IO a) => a -> AppState -> Application
app connectionsPool state = serve api $ hoistServer api (nt state) $ server connectionsPool

userServer :: (DbRepository IO a) => a -> ServerT UserAPI AppHandler
userServer connectionsPool =
  fetchUsers connectionsPool
    :<|> fetchUser connectionsPool
    :<|> createUser connectionsPool

transactionsServer :: (DbRepository IO a) => a -> ServerT TransactionsAPI AppHandler
transactionsServer connectionsPool =
  fetchTransactions connectionsPool
    :<|> addCreditTransaction connectionsPool
    :<|> addDebitTransaction connectionsPool

server :: (DbRepository IO a) => a -> ServerT API AppHandler
server connectionsPool =
  userServer connectionsPool
    :<|> transactionsServer connectionsPool
    :<|> getVersion

fetchUsers :: DbRepository IO a => a -> AppHandler [User]
fetchUsers conn = liftIO $ getAllUsers conn

fetchUser :: DbRepository IO a => a -> UserId -> AppHandler User
fetchUser conn userId = liftIO (getUserById conn userId) >>= notFoundResponse

createUser :: DbRepository IO a => a -> User -> AppHandler User
createUser conn user = do
  mu <- liftIO $ insertUser conn user
  case mu of
    Just u -> return u
    Nothing -> throwError err404

fetchTransactions :: DbRepository IO a => a -> UserId -> AppHandler [Transaction]
fetchTransactions conn userId = do
  users <- liftIO $ getTransactions conn userId
  case users of
    [] -> throwError err404
    a -> return a

addCreditTransaction :: (DbRepository IO a) => a -> UserId -> Amount -> AppHandler Transaction
addCreditTransaction conn userId amount = do
  result <- liftIO $ createCreditTransaction conn userId amount
  state <- ask
  case result of
    CreditUserNotFound -> throwError err404
    CorrectCredit t -> liftIO $ forkIO (void (increment state userId $ trxAmount t)) >> return t

addDebitTransaction :: (DbRepository IO a) => a -> UserId -> Amount -> AppHandler Transaction
addDebitTransaction conn userId amount = do
  result <- liftIO $ createDebitTransaction conn userId amount
  state <- ask
  case result of
    DebitUserNotFound -> throwError err404
    IncorrectAmount -> throwError err403
    CorrectDebit t -> liftIO $ forkIO (void (increment state userId $ trxAmount t)) >> return t

getVersion :: AppHandler String
getVersion = return "0.1.0.2"

notFoundResponse :: Maybe a -> AppHandler a
notFoundResponse Nothing = throwError err404
notFoundResponse (Just r) = return r
