{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module LocalConfig where

import Data.Aeson
import GHC.Generics

data AppConfig = AppConfig {
    db :: DbConfig
} deriving (Show, Generic)

data DbConfig = DbConfig {
    connectionString :: String
} deriving (Show, Generic)

instance FromJSON AppConfig
instance FromJSON DbConfig