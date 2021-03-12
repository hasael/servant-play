{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts#-} 


module Lib
    ( startApp
    , app
    , merge_
    , startAligner
    ) where


import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import DbRepository ( DbRepository )

import qualified Aligner as A
import qualified AppServer as S 

merge_ :: (DbRepository IO a) => a -> IO ()
merge_ = A.merge_

startAligner :: (DbRepository IO a)  => a -> IO ()
startAligner = A.start_

startApp :: Int -> Application -> IO ()
startApp port app = run port $ logStdoutDev app

app ::  (DbRepository IO a)  => a -> Application
app  = S.app