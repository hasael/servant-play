{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppAPI where
import Servant
import Models


type UserAPI = "users" :>
    (                              Get  '[JSON] [User]
     :<|> Capture "id" UserId      :> Get  '[JSON] User
     :<|> ReqBody '[JSON] User  :> Post '[JSON] User 
    )

type TransactionsAPI = "trx" :>
    (Capture "userId" UserId   :>    Get  '[JSON] [Transaction]
    :<|>  "credit" :> Capture "userId" UserId   :> Capture "amount" Double   :>    Post  '[JSON] Transaction
    :<|>  "debit" :> Capture "userId" UserId   :> Capture "amount" Double   :>    Post  '[JSON] Transaction )

type API = UserAPI :<|> TransactionsAPI