{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TestBase where
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Aeson
import Data.Aeson.Types
import Network.Wai.Test
import Data.ByteString.Char8 ( unpack )
import Models as M
import Data.Maybe

class (Monad m) => CanPropertyTest m where
    toProperty :: m Property -> Property

monadicProp :: (CanPropertyTest m) => PropertyM m () -> Property
monadicProp = monadic toProperty 

idFromUserResponse :: SResponse -> Int
idFromUserResponse resp = fromJust $ (M.id :: M.User -> Int) <$> decode (simpleBody resp)

idFromTrxResponse :: SResponse -> Int
idFromTrxResponse resp = fromJust $ (M.id :: M.Transaction -> Int) <$> decode (simpleBody resp)

--decode :: FromJSON a => LB.ByteString -> Maybe a