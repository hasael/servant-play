{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib
  ( startApp,
    app,
    merge_,
    startAligner,
    newState,
    MyHandler
  )
where

import qualified Aligner as A
import qualified AppServer as S
import DbRepository 
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Models ( AppState, EnvHandler, Transaction (userId), HasAppState, MyHandler )
import Control.Concurrent.STM
import Data.Map
import Control.Monad.Trans.Reader as R ( ReaderT(runReaderT), ask )
import Servant ( Handler, Application, hoistServer, serve, runHandler )
import Control.Monad.Reader

merge_ :: (DbRepository (EnvHandler env) env, HasAppState env) => env -> IO ()
merge_ = runReaderT A.merge_

startAligner ::  (DbRepository (EnvHandler env) env, HasAppState env) => env -> IO ()
startAligner = runReaderT A.start_

startApp :: Int -> Application -> IO ()
startApp port app = run port $ logStdoutDev app
 
nt :: env -> MyHandler env a -> Handler a
nt s x = runReaderT x s

app :: (DbRepository (MyHandler env) env, HasAppState env) => env -> Application
app env = serve S.api $ hoistServer S.api (nt env) S.server

newState :: IO AppState 
newState = atomically $ newTVar empty

