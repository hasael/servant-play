{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AppConfig where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

newtype AppConfig = AppConfig
  { db :: DbConfig
  }
  deriving (Show, Generic)

newtype DbConfig = DbConfig
  { connectionString :: String
  }
  deriving (Show, Generic)

instance FromJSON AppConfig

instance FromJSON DbConfig
