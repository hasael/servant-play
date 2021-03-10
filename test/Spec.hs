{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Network.Wai
import Network.Wai.Test(assertStatus, SResponse (simpleStatus))
import Network.HTTP.Types
import RealTestDb
import Properties
import Test.QuickCheck
import TestBase
import Data.ByteString.Char8 ( unpack, pack, isInfixOf )
import Data.ByteString.Lazy (toStrict)  
import qualified Data.ByteString.Lazy as LB(ByteString) 
import qualified Data.ByteString.Char8 as B( ByteString) 
import Data.Aeson ( encode )
import Models
import GHC.Int ( Int64 )

main :: IO ()
main = do 
    pool <- initTestDbConnection "host=localhost port=5435 dbname=postgres user=postgres password=playground"
    cleanTables pool
    let myapp = app pool
    hspec $ do
      spec myapp
      describe "Real DbRepository" $ do
          it "can create user" $ 
              property $ monadicPropIO . prop_insert_any_user pool                    
          it "creates and reads a user" $ 
              property $ monadicPropIO . prop_get_insert_user pool


spec :: Application -> Spec
spec app = with (return app ) $ do
    describe "POST /user" $ do
        it "creates and responds with 200" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0

             resp <- postJson "/users" $ encode userToCreate
             let createdUser = strictEncode $ userToCreate `withId` idFromUserResponse resp
             
             return resp `shouldRespondWith` 200
             return resp `bodyShouldEqual` createdUser

    describe "GET /user" $ do
        it "reads correct user" $ do
             let userToCreate = User 1 "Edmond" "Halley" 0
             resp <- postJson "/users" $ encode userToCreate

             let createdUserId = pack . show $ idFromUserResponse resp
             let createdUser = strictEncode $ userToCreate `withId` idFromUserResponse resp

             get ("/users/" <> createdUserId) `bodyShouldEqual` createdUser

    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with correct [User]" $ do
             let firstUserToCreate = User 1 "Johannes" "Kepler" 0
             let secondUserToCreate = User 1 "Nicola" "Copernicus" 0

             firstResp <- postJson "/users" $ encode firstUserToCreate
             secondResp <- postJson "/users" $ encode secondUserToCreate

             let firstCreatedUser = strictEncode $ firstUserToCreate `withId` idFromUserResponse firstResp
             let secondCreatedUser = strictEncode $ secondUserToCreate `withId` idFromUserResponse secondResp

             resp <- get "/users/"
             return resp `bodyShouldContain` firstCreatedUser
             return resp `bodyShouldContain` secondCreatedUser


postJson :: B.ByteString -> LB.ByteString -> WaiSession st SResponse
postJson path = request methodPost path [("Content-Type","application/json")] 

bodyShouldContain :: WaiSession () SResponse -> B.ByteString -> WaiExpectation ()
bodyShouldContain = responseComparer $ \a b -> a `isInfixOf` toStrict b

bodyShouldEqual :: WaiSession () SResponse -> B.ByteString -> WaiExpectation ()
bodyShouldEqual = responseComparer $ \a b -> a == toStrict b

responseComparer :: (B.ByteString -> LB.ByteString -> Bool) -> WaiSession () SResponse -> B.ByteString -> WaiExpectation () 
responseComparer fun context search = shouldRespondWith context $ ResponseMatcher 200 [] (MatchBody (\h b-> if not $ search `fun` b  then
                                                                    Just "response does not match"
                                                                    else Nothing ))