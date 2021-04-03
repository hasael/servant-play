{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AppAPI where

import Models
import Servant

type UserAPI =
  "users"
    :> ( Get '[JSON] [User]
           :<|> Capture "id" UserId :> Get '[JSON] User
           :<|> ReqBody '[JSON] User :> Post '[JSON] User
       )

type TransactionsAPI =
  "trx"
    :> ( Capture "userId" UserId :> Get '[JSON] [Transaction]
           :<|> "credit" :> Capture "userId" UserId :> Capture "amount" Amount :> Post '[JSON] Transaction
           :<|> "debit" :> Capture "userId" UserId :> Capture "amount" Amount :> Post '[JSON] Transaction
       )
type VersionAPI = "version" :> Get '[JSON] String
type API = UserAPI :<|> TransactionsAPI :<|> VersionAPI
