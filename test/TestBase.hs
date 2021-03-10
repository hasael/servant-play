{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TestBase where
import Test.QuickCheck ( Property )
import Test.QuickCheck.Monadic
import Data.Aeson
import Network.Wai.Test
import Data.ByteString.Char8 ( unpack , ByteString)
import Data.ByteString.Lazy (toStrict) 
import Models as M
import Data.Maybe ( fromJust )

class (Monad m) => CanPropertyTest m where
    toProperty :: m Property -> Property

monadicProp :: (CanPropertyTest m) => PropertyM m () -> Property
monadicProp = monadic toProperty 

idFromUserResponse :: SResponse -> Int
idFromUserResponse resp = fromJust $ (M.id :: M.User -> Int) <$> decode (simpleBody resp)

idFromTrxResponse :: SResponse -> Int
idFromTrxResponse resp = fromJust $ (M.id :: M.Transaction -> Int) <$> decode (simpleBody resp)

decodeTransaction :: SResponse -> M.Transaction 
decodeTransaction resp = fromJust $ decode (simpleBody resp)

withId :: User -> Int -> User
withId user userId = M.User userId (M.name user) (M.lastName user) ((M.amount :: M.User -> Double) user)

strictEncode :: ToJSON a => a -> ByteString
strictEncode a = toStrict $ encode a
