{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async (concurrently_)
import Control.Monad
import DBProperties
import Data.Aeson (encode)
import Data.ByteString.Char8 (isInfixOf, unpack)
import qualified Data.ByteString.Char8 as B (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as LB (ByteString)
import Data.List (delete)
import DbRepository
import GHC.Conc.IO
import GHC.Float
import GHC.Int (Int64)
import Lib (app, merge_, newState)
import MockedDb
import Models
import MonoidProperties
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test (SResponse (simpleBody, simpleStatus), assertStatus)
import RealTestDb
import System.Environment
import Test.Hspec
import Test.Hspec.Wai
  ( MatchBody (MatchBody),
    ResponseMatcher (ResponseMatcher),
    WaiExpectation,
    WaiSession,
    get,
    liftIO,
    request,
    shouldRespondWith,
    with,
  )
import Test.Hspec.Wai.Internal
  ( WaiExpectation,
    WaiSession,
    getApp,
    runWaiSession,
  )
import Test.QuickCheck
import TestBase

main :: IO ()
main = do
  as <- getArgs
  state <- newState
  if "real" `elem` as
    then do
      pool <- initTestDbConnection "host=localhost port=5437 dbname=postgres user=postgres password=playground"
      cleanTables pool
      withArgs (delete "real" as) $ runTests pool state
    else do
      db <- newDB
      runTests db state

runTests :: DbRepository IO a => a -> AppState -> IO ()
runTests c state = do
  let myapp = app c state
  hspec $ do
    apiSpec myapp
    concurrencySpecs c state myapp
    describe "DbRepository" $ do
      it "can create user" $
        property $ monadicPropIO . prop_insert_any_user c
      it "creates and reads a user" $
        property $ monadicPropIO . prop_user_read_after_insert c
      it "reads user idempotently" $
        property $ monadicPropIO . prop_user_idempotent_read c
      --      it "updates user correctly" $
      --        property $ \u a -> monadicPropIO $ prop_user_correct_amount_after_update c u a
      it "can create transaction" $
        quickCheck $ withMaxSuccess 1000 $ property $ \u a -> monadicPropIO $ prop_transaction_insert_any c u a
      it "creates and reads a transaction" $
        property $ \u a -> monadicPropIO $ prop_transaction_read_after_insert c u a
      it "reads transaction idempotently" $
        property $ \u a -> monadicPropIO $ prop_transaction_idempotent_read c u a
    describe "TransactionAmount is a monoid" $ do
      it "Associative" $
        quickCheck $ withMaxSuccess 1000 (prop_MonoidAssociativity :: TransactionAmount -> TransactionAmount -> TransactionAmount -> Bool)
      it "Right identity" $
        quickCheck $ property (prop_MonoidRightIdentity :: TransactionAmount -> Bool)
      it "Left identity" $
        quickCheck $ property (prop_MonoidLeftIdentity :: TransactionAmount -> Bool)

apiSpec :: Application -> Spec
apiSpec app = with (return app) $ do
  describe "POST /user" $ do
    it "response contains created user" $ do
      let userToCreate = User (mkUserId 1) "Isaac" "Newton" 0

      resp <- postJson "/users" $ encode userToCreate
      let createdUser = strictEncode $ userToCreate `withId` idFromUserResponse resp

      return resp `bodyShouldEqual` createdUser

  describe "GET /users/id" $ do
    it "responds with correct user" $ do
      let userToCreate = User (mkUserId 1) "Edmond" "Halley" 0
      resp <- postJson "/users" $ encode userToCreate
      let createdUserId = toByteString $ idFromUserResponse resp
      let createdUser = strictEncode $ userToCreate `withId` idFromUserResponse resp
      get ("/users/" <> createdUserId) `bodyShouldEqual` createdUser

  describe "GET /users" $ do
    it "responds with 200" $ do
      get "/users" `shouldRespondWith` 200
    it "responds with correct [User]" $ do
      let firstUserToCreate = User (mkUserId 1) "Johannes" "Kepler" 0
      let secondUserToCreate = User (mkUserId 1) "Nicola" "Copernicus" 0

      firstResp <- postJson "/users" $ encode firstUserToCreate
      secondResp <- postJson "/users" $ encode secondUserToCreate

      let firstCreatedUser = strictEncode $ firstUserToCreate `withId` idFromUserResponse firstResp
      let secondCreatedUser = strictEncode $ secondUserToCreate `withId` idFromUserResponse secondResp

      resp <- get "/users/"
      return resp `bodyShouldContain` firstCreatedUser
      return resp `bodyShouldContain` secondCreatedUser

  describe "POST /trx/credit/" $ do
    it "response contains credit transaction" $ do
      let userToCreate = User (mkUserId 1) "Isaac" "Newton" 0
      let myAmount = 10 :: Amount
      resp <- postJson "/users" $ encode userToCreate
      let createdUserId = idFromUserResponse resp
      let createdUserIdStr = toByteString createdUserId
      trxResp <- simplePost ("/trx/credit/" <> createdUserIdStr <> "/" <> toByteString myAmount)
      let createdTransaction = decodeTransaction trxResp
      liftIO $ transactionAmount createdTransaction `shouldBe` myAmount
      liftIO $ userId createdTransaction `shouldBe` idFromUserResponse resp

    it "responds with 404 for not existing user" $ do
      let myAmount = toByteString 10
      let createdUserId = toByteString 999999
      simplePost ("/trx/credit/" <> createdUserId <> "/" <> myAmount) `shouldRespondWith` 404

  describe "GET /trx/credit/" $ do
    it "response contains correct credit transaction" $ do
      let userToCreate = User (mkUserId 1) "Isaac" "Newton" 0

      resp <- postJson "/users" $ encode userToCreate
      let createdUserId = toByteString $ idFromUserResponse resp
      trxResp <- simplePost ("/trx/credit/" <> createdUserId <> "/10")
      get ("/trx/" <> createdUserId) `bodyShouldContain` toStrict (simpleBody trxResp)

  describe "POST /trx/debit/" $ do
    it "responds with 200 for user with amount" $ do
      let userToCreate = User (mkUserId 1) "Isaac" "Newton" 0
      let creditAmount = 10
      let debitAmount = 5
      resp <- postJson "/users" $ encode userToCreate
      let createdUserId = idFromUserResponse resp
      let createdUserIdStr = toByteString  createdUserId
      simplePost ("/trx/credit/" <> createdUserIdStr <> "/" <> toByteString creditAmount)
      trxResp <- simplePost ("/trx/debit/" <> createdUserIdStr <> "/" <> toByteString debitAmount)
      let createdTransaction = decodeTransaction trxResp
      liftIO $ transactionAmount createdTransaction `shouldBe` debitAmount
      liftIO $ userId createdTransaction `shouldBe` createdUserId

    it "responds with 403 for user without amount" $ do
      let userToCreate = User (mkUserId 1) "Isaac" "Newton" 0
      let creditAmount = toByteString 5
      let debitAmount = toByteString 6
      resp <- postJson "/users" $ encode userToCreate
      let createdUserId = toByteString $ idFromUserResponse resp
      simplePost ("/trx/credit/" <> createdUserId <> "/" <> creditAmount)
      simplePost ("/trx/debit/" <> createdUserId <> "/" <> debitAmount) `shouldRespondWith` 403

    it "responds with 404 for not existing user" $ do
      let myAmount = toByteString 10
      let createdUserId = toByteString 9999
      simplePost ("/trx/debit/" <> createdUserId <> "/" <> myAmount) `shouldRespondWith` 404

  describe "GET /trx/debit/" $ do
    it "response contains correct debit transaction" $ do
      let userToCreate = User (mkUserId 1) "Isaac" "Newton" 0
      let creditAmount = toByteString 10
      let debitAmount = toByteString 5
      resp <- postJson "/users" $ encode userToCreate
      let createdUserId = toByteString $ idFromUserResponse resp
      simplePost ("/trx/credit/" <> createdUserId <> "/" <> creditAmount)
      trxResp <- simplePost ("/trx/debit/" <> createdUserId <> "/" <> debitAmount)
      get ("/trx/" <> createdUserId) `bodyShouldContain` toStrict (simpleBody trxResp)

  describe "GET /user/ amount" $ do
    it "response contains correct debit transaction" $ do
      let userToCreate = User (mkUserId 1) "Isaac" "Newton" 0
      let creditAmount = 10
      let debitAmount = 5

      createUserResp <- postJson "/users" $ encode userToCreate
      let createdUserId = toByteString $ idFromUserResponse createUserResp
      simplePost ("/trx/credit/" <> createdUserId <> "/" <> toByteString creditAmount)
      firstGetUserResponse <- get ("/users/" <> createdUserId)
      simplePost ("/trx/debit/" <> createdUserId <> "/" <> toByteString debitAmount)
      secondGetUserResponse <- get ("/users/" <> createdUserId)
      let firstUserResp = decodeUser firstGetUserResponse
      let secondUserResp = decodeUser secondGetUserResponse
      liftIO $ userAmount firstUserResp `shouldBe` creditAmount
      liftIO $ userAmount secondUserResp `shouldBe` creditAmount - debitAmount

concurrencySpecs :: DbRepository IO a => a -> AppState -> Application -> Spec
concurrencySpecs conn state app = with (return app) $ do
  describe "GET /user/ amount on concurrent calls" $ do
    it "response contains correct credit transaction" $ do
      let userToCreate = User (mkUserId 1) "Isaac" "Newton" 0
      let creditAmount = 10 :: Amount
      createUserResp <- postJson "/users" $ encode userToCreate
      let createdUserId = toByteString $ idFromUserResponse createUserResp
      app <- getApp
      let concurrency = 10
      let expectedAmount = int2Double concurrency * creditAmount
      liftIO $ concurrentCallsN (simplePost ("/trx/credit/" <> createdUserId <> "/" <> toByteString creditAmount)) app concurrency
      liftIO $ threadDelay 100000
      liftIO $ merge_ conn state
      getUserResponse <- get ("/users/" <> createdUserId)
      let userResponse = decodeUser getUserResponse
      liftIO $ userAmount userResponse `shouldBe` expectedAmount

postJson :: B.ByteString -> LB.ByteString -> WaiSession st SResponse
postJson path = request methodPost path [("Content-Type", "application/json")]

simplePost :: B.ByteString -> WaiSession st SResponse
simplePost path = request methodPost path [] ""

bodyShouldContain :: WaiSession () SResponse -> B.ByteString -> WaiExpectation ()
bodyShouldContain = responseComparer $ \a b -> a `isInfixOf` toStrict b

bodyShouldEqual :: WaiSession () SResponse -> B.ByteString -> WaiExpectation ()
bodyShouldEqual = responseComparer $ \a b -> a == toStrict b

responseComparer :: (B.ByteString -> LB.ByteString -> Bool) -> WaiSession () SResponse -> B.ByteString -> WaiExpectation ()
responseComparer fun context search =
  shouldRespondWith context $
    ResponseMatcher
      200
      []
      ( MatchBody
          ( \h b ->
              if not $ search `fun` b
                then Just "response does not match"
                else Nothing
          )
      )

concurrentCalls :: WaiSession () SResponse -> Application -> IO ()
concurrentCalls call app =
  let twoCalls = concurrently_ (runWaiSession call app) (runWaiSession call app)
   in concurrently_ twoCalls twoCalls

concurrentCallsN :: WaiSession () SResponse -> Application -> Int -> IO ()
concurrentCallsN call app no = go call app 0
  where
    go c a n =
      when (n < no) $ concurrently_ (runWaiSession call app) (go c a (n + 1))
