{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai
import Network.HTTP.Types
import Data.Void
import TestDb
import Properties
import Data.Pool
import Database.PostgreSQL.Simple
import Test.QuickCheck

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
        it "creates with 200" $ do
             request methodPost "/users" [("Content-Type","application/json")] "{\"id\":1,\"name\":\"Isaac\",\"lastName\":\"Newton\",\"amount\":0}"  `shouldRespondWith` 200
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users/1" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "{\"id\":1,\"name\":\"Isaac\",\"lastName\":\"Newton\",\"amount\":0}"
            get "/users/1" `shouldRespondWith` users

