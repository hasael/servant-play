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
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Data.Pool
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import GHC.Generics
import Network.Wai.Middleware.RequestLogger

type UserId = Int 

data User = User
  { 
    id       :: Int,
    name     :: String,
    lastName :: String,
    amount   :: Double
  } deriving (Eq, Show, Generic)

data Transaction = Transaction
  { 
    id       :: Int,
    userId   :: Int,
    amount   :: Double,
    transactionType :: TransactionType
  } deriving (Eq, Show, Generic)

data TransactionType = Debit | Credit
      deriving (Eq, Show, Generic)

-- $(deriveJSON defaultOptions ''User)
type UserAPI = "users" :>
    (                              Get  '[JSON] [User]
     :<|> Capture "id" Int      :> Get  '[JSON] (User)
     :<|> ReqBody '[JSON] User  :> Post '[JSON] NoContent 
    )

type TransactionsAPI = "trx" :>
    (
          Capture "id" Int   :>    Get  '[JSON] (Transaction)
    )

type API = UserAPI :<|> TransactionsAPI

instance FromRow User
instance FromJSON User
instance ToJSON User

instance FromRow Transaction
instance ToJSON Transaction
instance ToJSON TransactionType

instance FromField TransactionType where
  fromField f (Just "Debit") = return Debit
  fromField f _ = return Credit


startApp :: Pool Connection -> IO ()
startApp connectionsPool = run 8080 $ (logStdoutDev . app) connectionsPool

app :: Pool Connection -> Application
app connectionsPool = serve api $ server connectionsPool

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server connectionsPool = (withResource connectionsPool fetchUsers 
                         :<|>  (\id -> (withResource connectionsPool $ \conn -> fetchUser conn id))
                         :<|>  (\u -> (withResource connectionsPool $ \conn -> insertUser conn u)))
                         :<|>  (\trxId -> (withResource connectionsPool $ \conn -> fetchTransaction conn trxId))

fetchUsers :: Connection -> Handler[User]
fetchUsers conn = liftIO $ query_ conn "SELECT id, name, last_name, amount from users"

fetchUser :: Connection -> Int -> Handler User
fetchUser conn  userId = do
                          users <- liftIO $ query conn "SELECT id, name, last_name, amount from users where id = ?" (Only userId) 
                          case users of [] -> throwError err404
                                        (x:_) -> return x

fetchTransaction :: Connection -> Int -> Handler Transaction
fetchTransaction conn  transactonId = do
                          users <- liftIO $ query conn "SELECT id, user_id, amount, transaction_type  from transactions where id = ?" (Only transactonId) 
                          case users of [] -> throwError err404
                                        (x:_) -> return x

insertUser :: Connection -> User -> Handler NoContent
insertUser conn user = liftIO $ execute conn "INSERT INTO users VALUES (default,?,?,0)" [name (user::User), lastName user] >> return NoContent
