{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Aeson
import Servant
import Servant.API

type Api = "user" :> QueryParam "name" String :> Get '[JSON] (Maybe User)
      :<|> "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (String)

data User = User {
  name :: String
, age :: Int
} deriving (Eq,Read,Show)

instance ToJSON User where
  toJSON (User {..}) = object [
      "name" .= name
    , "age"  .= age
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .: "name"
         <*> o .: "age"
