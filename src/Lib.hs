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
{-# LANGUAGE FlexibleContexts           #-} 


-- This is an unfortunate hack.  Used to make the code slightly easier to
-- follow.  See below for how we could fix it.
{-# LANGUAGE UndecidableInstances       #-}

-- This is another unfortunate hack to make the code simpler and easier to
-- understand.  Described at the end of this file.
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib
    ( startApp
    , app
    ) where


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
import DbRepository

type UserAPI = "users" :>
    (                              Get  '[JSON] [User]
     :<|> Capture "id" Int      :> Get  '[JSON] User
     :<|> ReqBody '[JSON] User  :> Post '[JSON] User 
    )

type TransactionsAPI = "trx" :>
    (
      Capture "userId" Int   :>    Get  '[JSON] [Transaction]
    :<|>  "credit" :> Capture "userId" Int   :> Capture "amount" Double   :>    Post  '[JSON] Transaction
    :<|>  "debit" :> Capture "userId" Int   :> Capture "amount" Double   :>    Post  '[JSON] Transaction
    )

type API = UserAPI :<|> TransactionsAPI

startApp :: DbRepository IO a =>  a-> IO ()
startApp connectionsPool = run 8080 $ (logStdoutDev . app) connectionsPool

app ::  DbRepository IO a => a -> Application
app connectionsPool = serve api $ server connectionsPool

api :: Proxy API
api = Proxy

server :: DbRepository IO a =>  a -> Server API
server connectionsPool = (fetchUsers connectionsPool
                         :<|>  fetchUser connectionsPool
                         :<|>  insertUser2 connectionsPool)
                         :<|>  (
                            fetchTransactions connectionsPool
                         :<|>  insertCreditTransaction2 connectionsPool
                         :<|>  insertDebitTransaction2 connectionsPool
                         )

fetchUsers :: DbRepository IO a =>  a -> Handler [User]
fetchUsers conn = liftIO $ getUsers conn 

notFoundResponse :: Maybe a -> Handler a
notFoundResponse Nothing = throwError err404
notFoundResponse (Just r) = return r


fetchUser ::  DbRepository IO a => a -> Int -> Handler User
fetchUser conn userId = liftIO ( getUser conn userId )>>= notFoundResponse

fetchTransaction :: DbRepository IO a => a  -> Int -> Handler Transaction
fetchTransaction conn transactionId = liftIO (getTransaction conn transactionId) >>= notFoundResponse

insertUser2 :: DbRepository IO a =>  a -> User -> Handler User
insertUser2 conn user = do
    mu <- liftIO $ createUser conn user
    case mu of 
      Just u -> return u
      Nothing -> throwError err404 

insertCreditTransaction2 :: DbRepository IO a => a  -> Int -> Double -> Handler Transaction
insertCreditTransaction2 conn userId amount = do 
                           result <- liftIO $ createCreditTransaction conn userId amount
                           case result of
                             CreditUserNotFound -> throwError err404
                             CorrectCredit t -> return t
                         

insertDebitTransaction2 :: DbRepository IO a => a  -> Int -> Double -> Handler Transaction
insertDebitTransaction2  conn userId amount =  do 
                           result <- liftIO $ createDebitTransaction conn userId amount
                           case result of
                             DebitUserNotFound  -> throwError err404
                             IncorrectAmount -> throwError err403
                             CorrectDebit t -> return t
                            
fetchTransactions :: DbRepository IO a => a  -> Int -> Handler [Transaction]
fetchTransactions conn userId =  do
                          users <- liftIO $ getUserTransactions conn userId
                          case users of [] -> throwError err404
                                        a -> return a