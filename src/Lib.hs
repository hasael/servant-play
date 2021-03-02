{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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


-- $(deriveJSON defaultOptions ''User)
type API = "users" :>
    (                              Get  '[JSON] [User]
     :<|> Capture "id" Int   :> Get  '[JSON] (User)
     :<|> ReqBody '[JSON] User  :> Post '[JSON] NoContent 
    )

instance FromRow User
instance FromJSON User
instance ToJSON User

startApp :: Pool Connection -> IO ()
startApp connectionsPool = run 8080 $ (logStdoutDev . app) connectionsPool

app :: Pool Connection -> Application
app connectionsPool = serve api $ server connectionsPool

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server connectionsPool = withResource connectionsPool fetchUsers 
                         :<|>  (\id -> (withResource connectionsPool $ \conn -> fetchUser conn id))
                         :<|>  (\u -> (withResource connectionsPool $ \conn -> insertUser conn u))

fetchUsers :: Connection -> Handler[User]
fetchUsers conn = liftIO $ query_ conn "SELECT id, name, last_name, amount from users"

fetchUser :: Connection -> Int -> Handler User
fetchUser conn  userId = do
                          users <- liftIO $ query conn "SELECT id, name, last_name, amount from users where id = ?" (Only userId) 
                          case users of [] -> throwError err404
                                        (x:_) -> return x

insertUser :: Connection -> User -> Handler NoContent
insertUser conn user = liftIO $ execute conn "INSERT INTO users VALUES (default,?,?,0)" [name user, lastName user] >> return NoContent
