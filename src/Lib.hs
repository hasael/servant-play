{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server
import Database.PostgreSQL.Simple ( Connection )
import Data.Pool
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import GHC.Generics
import Network.Wai.Middleware.RequestLogger
import Models
import UserService
import TransactionService

type UserAPI = "users" :>
    (                              Get  '[JSON] [User]
     :<|> Capture "id" Int      :> Get  '[JSON] User
     :<|> ReqBody '[JSON] User  :> Post '[JSON] NoContent 
    )

type TransactionsAPI = "trx" :>
    (
          Capture "id" Int   :>    Get  '[JSON] Transaction
    :<|>  Capture "userId" Int   :>    Get  '[JSON] [Transaction]
    :<|>  "credit" :> Capture "userId" Int   :> Capture "amount" Double   :>    Post  '[JSON] NoContent
    :<|>  "debit" :> Capture "userId" Int   :> Capture "amount" Double   :>    Post  '[JSON] NoContent
    )

type API = UserAPI :<|> TransactionsAPI

instance FromJSON User
instance ToJSON User

instance ToJSON Transaction
instance ToJSON TransactionType

startApp :: Pool Connection -> IO ()
startApp connectionsPool = run 8080 $ (logStdoutDev . app) connectionsPool

app :: Pool Connection -> Application
app connectionsPool = serve api $ server connectionsPool

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server connectionsPool = (withResource connectionsPool fetchUsers 
                         :<|>  (\id -> withResource connectionsPool $ \conn -> fetchUser conn id)
                         :<|>  (\u -> withResource connectionsPool $ \conn -> insertUser conn u))
                         :<|>  (
                           (\trxId -> withResource connectionsPool $ \conn -> fetchTransaction conn trxId)
                         :<|>  (\id  -> withResource connectionsPool $ \conn -> fetchTransactions conn id )
                         :<|>  (\id amount -> withResource connectionsPool $ \conn -> insertCreditTransaction conn id amount)
                         :<|>  (\id amount -> withResource connectionsPool $ \conn -> insertDebitTransaction conn id amount)
                         )

fetchUsers :: Connection -> Handler[User]
fetchUsers conn = liftIO $ getUsers conn

notFoundResponse :: Maybe a -> Handler a
notFoundResponse Nothing = throwError err404
notFoundResponse (Just r) = return r


fetchUser :: Connection -> Int -> Handler User
fetchUser conn userId = liftIO ( getUser conn userId )>>= notFoundResponse

fetchTransaction :: Connection -> Int -> Handler Transaction
fetchTransaction conn transactionId = liftIO (getTransaction conn transactionId) >>= notFoundResponse

insertUser :: Connection -> User -> Handler NoContent
insertUser conn user = liftIO $ createUser conn user >> return NoContent

insertCreditTransaction :: Connection -> Int -> Double -> Handler NoContent
insertCreditTransaction conn userId amount = do 
                           result <- liftIO $ createCreditTransaction conn userId amount
                           case result of
                             CreditUserNotFound -> throwError err404
                             CorrectCredit -> return NoContent
                         

insertDebitTransaction :: Connection -> Int -> Double -> Handler NoContent
insertDebitTransaction  conn userId amount =  do 
                           result <- liftIO $ createDebitTransaction conn userId amount
                           case result of
                             DebitUserNotFound  -> throwError err404
                             IncorrectAmount -> throwError err403
                             CorrectDebit -> return NoContent
                            
fetchTransactions :: Connection -> Int -> Handler [Transaction]
fetchTransactions conn userId =  do
                          users <- liftIO $ getUserTransactions conn userId
                          case users of [] -> throwError err404
                                        a -> return a