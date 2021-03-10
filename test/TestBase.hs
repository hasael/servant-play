{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TestBase where
import Test.QuickCheck ( Property )
import Test.QuickCheck.Monadic
import Data.Aeson
import Network.Wai.Test
import Data.ByteString.Char8 as B (pack, unpack , ByteString)
import Data.ByteString.Lazy as L (toStrict, ByteString) 
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


decodeUser:: SResponse -> M.User 
decodeUser resp = fromJust $ decode (simpleBody resp)

withId :: User -> Int -> User
withId user userId = M.User userId (M.name user) (M.lastName user) ((M.amount :: M.User -> Double) user)

strictEncode :: ToJSON a => a -> B.ByteString
strictEncode a = toStrict $ encode a

toByteString ::Show a => a -> B.ByteString 
toByteString = pack . show