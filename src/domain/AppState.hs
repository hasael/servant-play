{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Domain.AppState where

import Data.Aeson (FromJSON, ToJSON)
import Data.Decimal
import Database.PostgreSQL.Simple.FromField ( FromField(fromField) )
import Database.PostgreSQL.Simple.ToField
import GHC.Base (Double, Eq, Int, Ord, String, undefined, error, IO)
import GHC.Float.ConversionUtils
import GHC.Generics
import GHC.Show 
import Data.Map
import GHC.Conc.Sync
import Servant
import Prelude as P (Float, Fractional, Integral, Real (toRational), realToFrac, (&&), (+), (<), (==), (>) , ($), (<$>) ,(.))
import Refined
import Data.Either
import Data.Either.Combinators (mapLeft)
import qualified Data.Text as T
import Control.Monad.Reader  
import Domain.User
import Domain.Transaction


type AppState = TVar (Map UserId [TransactionAmount])

type MyHandler env = ReaderT env Handler

type EnvHandler env = ReaderT env IO

class HasAppState a where
  getAppState :: a -> AppState
