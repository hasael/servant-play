{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module TestBase where

import Data.Aeson
import Data.ByteString.Char8 as B (ByteString, pack, unpack)
import Data.ByteString.Lazy as L (ByteString, toStrict)
import Data.Maybe (fromJust)
import Models
import Network.Wai.Test
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic
import Refined
import Data.Either

class (Monad m) => CanPropertyTest m where
  toProperty :: m Property -> Property

monadicProp :: (CanPropertyTest m) => PropertyM m () -> Property
monadicProp = monadic toProperty

transactionAmount :: Transaction -> Amount
transactionAmount = amount

idFromUserResponse :: SResponse -> UserId
idFromUserResponse resp = fromJust $ getUserId <$> decode (simpleBody resp)

idFromTrxResponse :: SResponse -> TransactionId
idFromTrxResponse resp = fromJust $ getTransactionId <$> decode (simpleBody resp)

decodeTransaction :: SResponse -> Transaction
decodeTransaction resp = fromJust $ decode (simpleBody resp)

decodeUser :: SResponse -> User
decodeUser resp = fromJust $ decode (simpleBody resp)

withId :: User -> UserId -> User
withId user userId = User userId (name user) (lastName user) ((amount :: User -> Amount) user)

withAmount ::  Amount -> User -> User
withAmount amount user = User (getUserId user) (name user) (lastName user) amount

strictEncode :: ToJSON a => a -> B.ByteString
strictEncode a = toStrict $ encode a

toByteString :: Show a => a -> B.ByteString
toByteString = pack . show

defaultUser :: UserId -> User 
defaultUser usrId = User usrId "Haskell" "Curry" 100

mkUserId :: Int -> UserId
mkUserId id =  UserId $ fromRight (error "invalid userId") $ refine id