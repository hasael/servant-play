{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Lib (app, merge_)
import Test.Hspec
import Test.Hspec.Wai
    ( liftIO,
      request,
      get,
      request,
      shouldRespondWith,
      with,
      WaiExpectation,
      WaiSession,
      MatchBody(MatchBody),
      ResponseMatcher(ResponseMatcher) )

import Test.Hspec.Wai.Internal
    ( WaiExpectation, WaiSession, runWaiSession, getApp )
import Network.Wai
import Network.Wai.Test(assertStatus, SResponse (simpleStatus, simpleBody))
import Network.HTTP.Types
import RealTestDb
import Properties
import Test.QuickCheck
import TestBase
import Data.ByteString.Char8 ( unpack, isInfixOf )
import Data.ByteString.Lazy (toStrict)  
import qualified Data.ByteString.Lazy as LB(ByteString) 
import qualified Data.ByteString.Char8 as B( ByteString) 
import Data.Aeson ( encode )
import Models
import GHC.Int ( Int64 )
import GHC.Float ( int2Double )
import Control.Concurrent.Async ( concurrently_ )
import Control.Monad
import Instances
import DbRepository

main :: IO ()
main = do 
    pool <- initTestDbConnection "host=localhost port=5435 dbname=postgres user=postgres password=playground"
    cleanTables pool
    let myapp = app pool
    hspec $ do
      spec myapp
      concurrencySpecs pool myapp
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

    describe "GET /users/" $ do
        it "responds with correct user" $ do
             let userToCreate = User 1 "Edmond" "Halley" 0
             resp <- postJson "/users" $ encode userToCreate

             let createdUserId = toByteString $ idFromUserResponse resp
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
             let myAmount = 10 :: Double
             resp <- postJson "/users" $ encode userToCreate
             let createdUserId = idFromUserResponse resp
             let createdUserIdStr = toByteString createdUserId
             trxResp <- simplePost ("/trx/credit/" <> createdUserIdStr <> "/" <> toByteString myAmount )
             let createdTransaction = decodeTransaction trxResp
             liftIO $ transactionAmount createdTransaction `shouldBe` myAmount
             liftIO $ userId createdTransaction `shouldBe` idFromUserResponse resp

        it "responds with 404 for not existing user" $ do
             let myAmount = toByteString 10
             let createdUserId = toByteString $ -1
             simplePost ("/trx/credit/" <> createdUserId <> "/" <> myAmount ) `shouldRespondWith` 404


    describe "GET /trx/credit/" $ do
        it "response contains correct credit transaction" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0

             resp <- postJson "/users" $ encode userToCreate
             let createdUserId = toByteString $ idFromUserResponse resp
             trxResp <- simplePost ("/trx/credit/" <> createdUserId <> "/10" )
             get ("/trx/" <> createdUserId) `bodyShouldContain` toStrict (simpleBody trxResp)

    describe "POST /trx/debit/" $ do
        it "responds with 200 for user with amount" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0
             let creditAmount = 10
             let debitAmount = 5
             resp <- postJson "/users" $ encode userToCreate
             let createdUserId = idFromUserResponse resp
             simplePost ("/trx/credit/" <> toByteString createdUserId <> "/" <> toByteString creditAmount )
             trxResp <- simplePost ("/trx/debit/" <> toByteString createdUserId <> "/" <> toByteString debitAmount )
             let createdTransaction = decodeTransaction trxResp
             liftIO $ transactionAmount createdTransaction `shouldBe` debitAmount
             liftIO $ userId createdTransaction `shouldBe` createdUserId

        it "responds with 403 for user without amount" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0
             let creditAmount = toByteString 5
             let debitAmount = toByteString 6
             resp <- postJson "/users" $ encode userToCreate
             let createdUserId = toByteString $ idFromUserResponse resp
             simplePost ("/trx/credit/" <> createdUserId <> "/" <> creditAmount ) 
             simplePost ("/trx/debit/" <> createdUserId <> "/" <> debitAmount ) `shouldRespondWith` 403


        it "responds with 404 for not existing user" $ do
             let myAmount = toByteString 10
             let createdUserId = toByteString $ -1
             simplePost ("/trx/debit/" <> createdUserId <> "/" <> myAmount )`shouldRespondWith` 404


    describe "GET /trx/debit/" $ do
        it "response contains correct debit transaction" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0
             let creditAmount = toByteString 10
             let debitAmount = toByteString 5
             resp <- postJson "/users" $ encode userToCreate
             let createdUserId = toByteString $ idFromUserResponse resp
             simplePost ("/trx/credit/" <> createdUserId <> "/" <> creditAmount )
             trxResp <- simplePost ("/trx/debit/" <> createdUserId <> "/" <> debitAmount )
             get ("/trx/" <> createdUserId) `bodyShouldContain` toStrict (simpleBody trxResp)

    describe "GET /user/ amount" $ do
        it "response contains correct debit transaction" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0
             let creditAmount = 10
             let debitAmount = 5

             createUserResp <- postJson "/users" $ encode userToCreate
             let createdUserId = toByteString $ idFromUserResponse createUserResp
             simplePost ("/trx/credit/" <> createdUserId <> "/" <> toByteString creditAmount )
             firstGetUserResponse <- get ("/users/" <> createdUserId)
             simplePost ("/trx/debit/" <> createdUserId <> "/" <> toByteString debitAmount )
             secondGetUserResponse <- get ("/users/" <> createdUserId)
             let firstUserResp = decodeUser firstGetUserResponse
             let secondUserResp = decodeUser secondGetUserResponse
             liftIO $ userAmount firstUserResp `shouldBe` creditAmount
             liftIO $ userAmount secondUserResp `shouldBe` creditAmount - debitAmount
             
concurrencySpecs :: DbRepository IO a => a -> Application -> Spec
concurrencySpecs conn app = with (return app ) $ do
    describe "GET /user/ amount on concurrent calls" $ do
        it "response contains correct debit transaction" $ do
             let userToCreate = User 1 "Isaac" "Newton" 0
             let creditAmount = 10
             createUserResp <- postJson "/users" $ encode userToCreate
             let createdUserId = toByteString $ idFromUserResponse createUserResp
             app <- getApp 
             let concurrency = 10
             liftIO $ concurrentCallsN (simplePost ("/trx/credit/" <> createdUserId <> "/" <> toByteString creditAmount)) app concurrency
             liftIO $ merge_ conn
             getUserResponse <- get ("/users/" <> createdUserId)
             let userResponse = decodeUser getUserResponse
             liftIO $ userAmount userResponse `shouldBe` creditAmount * int2Double concurrency

postJson :: B.ByteString -> LB.ByteString -> WaiSession st SResponse
postJson path = request methodPost path [("Content-Type","application/json")] 

simplePost :: B.ByteString -> WaiSession st SResponse
simplePost path = request methodPost path [] "" 

bodyShouldContain :: WaiSession () SResponse -> B.ByteString -> WaiExpectation ()
bodyShouldContain = responseComparer $ \a b -> a `isInfixOf` toStrict b

bodyShouldEqual :: WaiSession () SResponse -> B.ByteString -> WaiExpectation ()
bodyShouldEqual = responseComparer $ \a b -> a == toStrict b

responseComparer :: (B.ByteString -> LB.ByteString -> Bool) -> WaiSession () SResponse -> B.ByteString -> WaiExpectation () 
responseComparer fun context search = shouldRespondWith context $ ResponseMatcher 200 [] (MatchBody (\h b-> if not $ search `fun` b  then
                                                                    Just "response does not match"
                                                                    else Nothing ))

concurrentCalls :: WaiSession () SResponse -> Application -> IO ()
concurrentCalls call app = let twoCalls = concurrently_ (runWaiSession call app) (runWaiSession call app)
                              in concurrently_ twoCalls twoCalls

concurrentCallsN :: WaiSession () SResponse -> Application -> Int -> IO ()
concurrentCallsN call app no = go call app 0
                               where go c a n =
                                       when ( n < no) $ concurrently_ (runWaiSession call app) (go c a (n+1))
                                     
