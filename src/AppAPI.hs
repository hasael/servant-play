{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AppAPI where
import Servant
import Models

type UserAPI = "users" :>
    (                              Get  '[JSON] [User]
     :<|> Capture "id" Int      :> Get  '[JSON] User
     :<|> ReqBody '[JSON] User  :> Post '[JSON] User 
    )

type TransactionsAPI = "trx" :>
    (Capture "userId" Int   :>    Get  '[JSON] [Transaction]
    :<|>  "credit" :> Capture "userId" Int   :> Capture "amount" Double   :>    Post  '[JSON] Transaction
    :<|>  "debit" :> Capture "userId" Int   :> Capture "amount" Double   :>    Post  '[JSON] Transaction )

type API = UserAPI :<|> TransactionsAPI