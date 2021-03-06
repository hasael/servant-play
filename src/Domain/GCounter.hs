{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.GCounter where

import Control.Concurrent.STM
import Control.Monad
import Data.Map
import Data.Maybe
import Data.Monoid
import Prelude (IO, Ord, ($))

class (Monoid m, Ord a) => GCounter m a where
  merge :: (GCounter m a) => TVar (Map a [m]) -> IO (Map a m)
  merge state = do
    atomically $ do
      currData <- readTVar state
      let result = fmap mconcat currData
      return result

  increment :: TVar (Map a [m]) -> a -> m -> IO (Map a [m])
  increment state key value = do
    atomically $ do
      curr <- readTVar state
      let currList = fromMaybe [] (lookup key curr) <> [value]
      let newValue = insert key currList curr
      writeTVar state newValue
      return newValue
