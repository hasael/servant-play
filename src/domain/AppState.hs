{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.AppState where

import Control.Monad.Reader (ReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Domain.Transaction (TransactionAmount)
import Domain.User (UserId)
import GHC.Base (IO)
import GHC.Conc.Sync (TVar)
import Servant (Handler)

type AppState = TVar (Map UserId [TransactionAmount])

type MyHandler env = ReaderT env Handler

type EnvHandler env = ReaderT env IO

class HasAppState a where
  getAppState :: a -> AppState
