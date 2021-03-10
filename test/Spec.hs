{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Network.Wai
import Network.Wai.Test(assertStatus, SResponse (simpleStatus, simpleBody))
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
        it "response contains created user" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0

             resp <- postJson "/users" $ encode userToCreate
             let createdUser = strictEncode $ userToCreate `withId` idFromUserResponse resp
             
             return resp `bodyShouldEqual` createdUser

    describe "GET /user" $ do
        it "responds with correct user" $ do
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

    describe "POST /trx/credit/" $ do
        it "response contains credit transaction" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0
             let myAmount = pack . show $ 10
             resp <- postJson "/users" $ encode userToCreate
             let createdUserId = pack . show $ idFromUserResponse resp
             trxResp <- request methodPost ("/trx/credit/" <> createdUserId <> "/" <> myAmount ) [] "" 
             --let createdTransaction = decodeTransaction trxResp
             return trxResp `bodyShouldContain` createdUserId
             return trxResp `bodyShouldContain` myAmount

        it "responds with 404 for not existing user" $ do
             let myAmount = pack . show $ 10
             let createdUserId = pack . show $ -1
             request methodPost ("/trx/credit/" <> createdUserId <> "/" <> myAmount ) [] "" `shouldRespondWith` 404


    describe "GET /trx/credit/" $ do
        it "response contains correct credit transaction" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0

             resp <- postJson "/users" $ encode userToCreate
             let createdUserId = pack . show $ idFromUserResponse resp
             trxResp <- request methodPost ("/trx/credit/" <> createdUserId <> "/10" ) [] "" 
             get ("/trx/" <> createdUserId) `bodyShouldContain` toStrict (simpleBody trxResp)

    describe "POST /trx/debit/" $ do
        it "responds with 200 for user with amount" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0
             let creditAmount = pack . show $ 10
             let debitAmount = pack . show $ 5
             resp <- postJson "/users" $ encode userToCreate
             let createdUserId = pack . show $ idFromUserResponse resp
             request methodPost ("/trx/credit/" <> createdUserId <> "/" <> creditAmount ) [] "" 
             trxResp <- request methodPost ("/trx/debit/" <> createdUserId <> "/" <> debitAmount ) [] "" 
             return trxResp `bodyShouldContain` createdUserId
             return trxResp `bodyShouldContain` debitAmount

        it "responds with 403 for user without amount" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0
             let creditAmount = pack . show $ 5
             let debitAmount = pack . show $ 6
             resp <- postJson "/users" $ encode userToCreate
             let createdUserId = pack . show $ idFromUserResponse resp
             request methodPost ("/trx/credit/" <> createdUserId <> "/" <> creditAmount ) [] "" 
             request methodPost ("/trx/debit/" <> createdUserId <> "/" <> debitAmount ) [] "" `shouldRespondWith` 403


        it "responds with 404 for not existing user" $ do
             let myAmount = pack . show $ 10
             let createdUserId = pack . show $ -1
             request methodPost ("/trx/debit/" <> createdUserId <> "/" <> myAmount ) [] "" `shouldRespondWith` 404


    describe "GET /trx/debit/" $ do
        it "response contains correct debit transaction" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0
             let creditAmount = pack . show $ 10
             let debitAmount = pack . show $ 5
             resp <- postJson "/users" $ encode userToCreate
             let createdUserId = pack . show $ idFromUserResponse resp
             request methodPost ("/trx/credit/" <> createdUserId <> "/" <> creditAmount ) [] "" 
             trxResp <- request methodPost ("/trx/debit/" <> createdUserId <> "/" <> debitAmount ) [] "" 
             get ("/trx/" <> createdUserId) `bodyShouldContain` toStrict (simpleBody trxResp)
             

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