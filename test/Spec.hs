{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai
import Network.Wai.Test(assertStatus, SResponse (simpleStatus))
import Network.HTTP.Types
import TestDb
import Properties
import Test.QuickCheck
import TestBase
import Data.ByteString.Char8 
import Data.ByteString.Lazy(toStrict)
import Data.String

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
        it "creates and reads with 200" $ do
             resp <- request methodPost "/users" [("Content-Type","application/json")] "{\"id\":1,\"name\":\"Isaac\",\"lastName\":\"Newton\",\"amount\":0}" 
             return resp `shouldRespondWith` 200
             let createdUserId = pack $ show $ idFromUserResponse resp
             get ("/users/" <> createdUserId ) `shouldRespondWith` 200
             let user = "{\"amount\":0,\"lastName\":\"Newton\",\"name\":\"Isaac\",\"id\":"<> createdUserId <>"}"
             liftIO $ print createdUserId
             get ("/users/"<> createdUserId) `shouldRespondWith` fromString (unpack user)

    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "\"lastName\":\"Newton\",\"name\":\"Isaac\""
            
            get "/users/" `shouldRespondWith` ResponseMatcher  200 [] (MatchBody (\h b-> if not $ users`isInfixOf` toStrict b then
                                                                    Just "response does not match"
                                                                    else Nothing ))

