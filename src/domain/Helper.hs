{-# LANGUAGE NoImplicitPrelude #-}

module Domain.Helper where

import Data.Either (fromRight)
import Data.Either.Combinators (mapLeft)
import Data.Text (pack)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.ToField
import GHC.Base (Double, error)
import Refined
import Servant
import Prelude (($), (.), (<$>))

type Amount = Double

instance (FromField a, Predicate p a) => FromField (Refined p a) where
  fromField f bs = fromRight (error "error on FromField") . refine <$> fromField f bs

instance (ToField a, Predicate p a) => ToField (Refined p a) where
  toField v = toField $ unrefine v

instance (FromHttpApiData a, Predicate p a) => FromHttpApiData (Refined p a) where
  parseUrlPiece t = do
    r <- parseUrlPiece t
    mapLeft (\l -> pack (displayRefineException l)) $ refine r
