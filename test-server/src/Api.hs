{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Aeson
import Data.Text (Text)
import Servant
import Servant.API

type Api = "user"              :> QueryParam "name" String :> Get '[JSON] (Maybe User)
      :<|> "user"  :> "add"    :> ReqBody '[JSON] User     :> Post '[JSON] (Maybe User)
      :<|> "user"  :> "delete" :> QueryParam "name" String :> Delete '[JSON] Bool
      :<|> "user"  :> "exists" :> QueryParam "name" String :> Get '[JSON] Bool
      :<|> "user"  :> "upsert" :> ReqBody '[JSON] User     :> Post '[JSON] User
      :<|> "users" :> "add"    :> ReqBody '[JSON] [User]   :> Post '[JSON] [User]
      :<|> "capture"          :> "test"    :> Capture "segment" Text    :> Get '[JSON] (Text)
      :<|> "capture" :> "all" :> "test"    :> CaptureAll "segment" Text :> Get '[JSON] [Text]

--       :<|> "capture"           :> "test" :> Capture "segment" Text      :> Get '[JSON] (Text)


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
