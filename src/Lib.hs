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
import UserRepository
import TransactionRepository

-- $(deriveJSON defaultOptions ''User)
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
                         :<|>  (\u -> withResource connectionsPool $ \conn -> insertUser2 conn u))
                         :<|>  (
                           (\trxId -> withResource connectionsPool $ \conn -> fetchTransaction conn trxId)
                         :<|>  (\id  -> withResource connectionsPool $ \conn -> fetchTransactions conn id )
                         :<|>  (\id amount -> withResource connectionsPool $ \conn -> insertCreditTransaction2 conn id amount)
                         :<|>  (\id amount -> withResource connectionsPool $ \conn -> insertDebitTransaction2 conn id amount)
                         )

fetchUsers :: Connection -> Handler[User]
fetchUsers conn = liftIO $ getAllUsers conn

notFoundResponse :: Maybe a -> Handler a
notFoundResponse Nothing = throwError err404
notFoundResponse (Just r) = return r


fetchUser :: Connection -> Int -> Handler User
fetchUser conn userId = liftIO ( getUser conn userId )>>= notFoundResponse

fetchTransaction :: Connection -> Int -> Handler Transaction
fetchTransaction conn transactionId = liftIO (getTransaction conn transactionId) >>= notFoundResponse

insertUser2 :: Connection -> User -> Handler NoContent
insertUser2 conn user = liftIO $ insertUser conn user >> return NoContent

insertCreditTransaction2 :: Connection -> Int -> Double -> Handler NoContent
insertCreditTransaction2 conn userId amount = liftIO $ do
                          insertCreditTransaction conn userId amount
                          curramount <- getUserAmount conn userId
                          let newAmount = (+amount) <$> curramount
                          case newAmount of 
                            Just a -> updateUserAmount conn userId a >>return NoContent 
                            Nothing -> return NoContent

insertDebitTransaction2 :: Connection -> Int -> Double -> Handler NoContent
insertDebitTransaction2  conn userId amount =  do                       
                          curramount <- liftIO $ getUserAmount conn userId
                          let newAmount = (\a -> a - amount) <$> curramount
                          case newAmount of 
                            Just a -> if a>=0 then liftIO $ do
                                         insertDebitTransaction conn userId amount
                                         updateUserAmount conn userId a >>return NoContent 
                                      else
                                        throwError err403
                            Nothing -> throwError err404
                            

fetchTransactions :: Connection -> Int -> Handler [Transaction]
fetchTransactions conn userId =  do
                          users <- liftIO $ getTransactions conn userId
                          case users of [] -> throwError err404
                                        a -> return a