{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Database.PostgreSQL.Simple
import Data.Pool
import Data.ByteString
import Control.Monad.IO.Class
import GHC.Generics
import Network.Wai.Middleware.RequestLogger

data User = User
  { 
    id        :: Rational
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

instance FromRow User

startApp :: Pool Connection -> IO ()
startApp connectionsPool = run 8080 $ (logStdoutDev . app) connectionsPool

app :: Pool Connection -> Application
app connectionsPool = serve api $ server connectionsPool

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server connectionsPool = withResource connectionsPool fetchUsers

fetchUsers :: Connection -> Handler[User]
fetchUsers conn = liftIO $ query_ conn "SELECT min as id FROM mileage.mileage_category where id =2"

users :: [User]
users = [ User 1 
        , User 2 
        ]
