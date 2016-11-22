{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Api
import           Control.Monad.Trans.Either
import           Servant.Common.Req (printServantError)
import           Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  interface <- runIO createApiInterface
  describe "ghcjs-servant-client" $ do
    it "GET QueryParam" $ do
      eitherUser <- runEitherT $ apiGetUser interface (Just "James")
      case eitherUser of
        Left  _ -> fail "GET QueryParam test failed."
        Right user -> user `shouldBe` (Just (User "James" 25))
    it "POST JSON ReqBody" $ do
      eitherUser <- runEitherT $ apiPostUser interface (User "James" 40)
      case eitherUser of
        Left  _ -> fail "POST JSON ReqBody failed."
        Right user -> user `shouldBe` (Just (User "James" 40))
    it "DELETE QueryParam" $ do
      eitherResult <- runEitherT $ apiDeleteUser interface (Just "James")
      case eitherResult of
        Left  _ -> fail "Delete QueryParam failed."
        Right r -> r `shouldBe` True
    it "GET QueryParam" $ do
      eitherResult <- runEitherT $ apiExistsUser interface (Just "James")
      case eitherResult of
        Left  _ -> fail "Get QueryParam failed."
        Right r -> r `shouldBe` True
    it "POST JSON ReqBody List" $ do
      eitherResult <- runEitherT $ apiPostUsers interface ([User "James" 40, User "Rick" 50])
      case eitherResult of
        Left  _ -> fail "POST JSON ReqBody List failed."
        Right r -> r `shouldBe` ([User "James" 40, User "Rick" 50])
    it "GET Capture Text" $ do
      eitherResult <- runEitherT $ apiGetCapture interface "This is only a test"
      case eitherResult of
        Left  err -> do
          printServantError err
          fail "GET Capture Text failed."
        Right r -> r `shouldBe` "This is only a test"
    it "GET CaptureAll Text" $ do
      eitherResult <- runEitherT $ apiGetCaptureAll interface ["1","2","Hello World!"]
      case eitherResult of
        Left  err -> do
          printServantError err
          fail "GET CaptureAll Text failed."
        Right r -> r `shouldBe` ["1","2","Hello World!"]
