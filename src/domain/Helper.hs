{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Domain.Helper where

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

type Amount = Double

instance (FromField a, Predicate p a) => FromField (Refined p a)  where
  fromField f bs = fromRight (error "error on FromField") .  refine <$> fromField f bs

instance (ToField a, Predicate p a) => ToField (Refined p a) where
  toField v = toField $ unrefine v

instance (FromHttpApiData a, Predicate p a) => FromHttpApiData (Refined p a) where
  parseUrlPiece t = do
    r <- parseUrlPiece t
    mapLeft (\l -> T.pack (displayRefineException l)) $ refine r
