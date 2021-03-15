{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GCounter where

import Control.Concurrent.STM
import Control.Monad
import Data.Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import System.IO.Unsafe
import Prelude as P (IO, Int, Show, foldr, fromInteger, print, putStrLn, show, ($), (+), (.))

class (Monoid m, Ord a, Show a, Show m) => GCounter m a where
  totals :: TVar (Map a m)
  totals = unsafePerformIO $ newTVarIO empty

  totalData :: TVar (Map a [m])
  totalData = unsafePerformIO $ newTVarIO empty

  merge :: (GCounter m a) => IO (Map a m)
  merge = do
    atomically $ do
      currData <- readTVar totalData
      let result = fmap mconcat currData
      writeTVar totals result
      return result

  increment :: a -> m -> IO (Map a [m])
  increment key value = do
    atomically $ do
      curr <- readTVar totalData
      let currList = fromMaybe [] (lookup key curr) <> [value]
      let newValue = insert key currList curr
      writeTVar totalData newValue
      return newValue
