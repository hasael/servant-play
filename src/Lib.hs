{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( startApp,
    app,
    merge_,
    startAligner,
    newState
  )
where

import qualified Aligner as A
import qualified AppServer as S
import DbRepository (DbRepository)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Models ( AppState )
import Control.Concurrent.STM
import Data.Map

merge_ :: (DbRepository IO a) => a -> AppState -> IO ()
merge_ = A.merge_

startAligner :: (DbRepository IO a) => a -> AppState -> IO ()
startAligner = A.start_

startApp :: Int -> Application -> IO ()
startApp port app = run port $ logStdoutDev app

app :: (DbRepository IO a) => a -> AppState -> Application
app = S.app

newState :: IO AppState 
newState = atomically $ newTVar empty