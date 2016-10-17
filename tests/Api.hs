{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


module Api where

import           Control.Monad
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class

import           Data.Aeson
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T

import           Servant.API
import           Servant.Client

import           GHCJS.JSVal.Combinators
import           GHCJS.Marshal

data ApiInterface = ApiInterface {
  apiGetUser    :: Maybe String -> EitherT ServantError IO (Maybe User)
, apiPostUser   :: User         -> EitherT ServantError IO (Maybe User)
, apiDeleteUser :: Maybe String -> EitherT ServantError IO (Bool)
, apiExistsUser :: Maybe String -> EitherT ServantError IO (Bool)
, apiUpsertUser :: User         -> EitherT ServantError IO (User)
}

type Api = "user"             :> QueryParam "name" String :> Get '[JSON] (Maybe User)
      :<|> "user" :> "add"    :> ReqBody    '[JSON] User  :> Post   '[JSON] (Maybe User)
      :<|> "user" :> "delete" :> QueryParam "name" String :> Delete '[JSON] Bool
      :<|> "user" :> "exists" :> QueryParam "name" String :> Get '[JSON] Bool
      :<|> "user" :> "upsert" :> ReqBody    '[JSON] User  :> Post '[JSON] User

-- the following doesn't compile
-- "user" :> "add" :> ReqBody '[JSON] User :> Post '[] ()

data User = User {
  name :: String
, age :: Int
} deriving (Eq,Read,Show)

createApiInterface :: IO (ApiInterface)
createApiInterface = do
  return $ ApiInterface apiGetUser' apiPostUser' apiDeleteUser' apiExistsUser' apiUpsertUser'
  where
    apiGetUser'    :: Maybe String -> EitherT ServantError IO (Maybe User)
    apiPostUser'   :: User         -> EitherT ServantError IO (Maybe User)
    apiDeleteUser' :: Maybe String -> EitherT ServantError IO (Bool)
    apiExistsUser' :: Maybe String -> EitherT ServantError IO (Bool)
    apiUpsertUser' :: User         -> EitherT ServantError IO (User)
    apiGetUser' :<|> apiPostUser' :<|>  apiDeleteUser' :<|> apiExistsUser' :<|> apiUpsertUser' = client api $ Just $ BaseUrl scheme url port
    api :: Proxy Api
    api = Proxy
    url = "127.0.0.1"
    port = 3000
    scheme = Http


instance ToJSON User where
  toJSON (User {..}) = object [
      "name" .= name
    , "age"  .= age
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .: "name"
         <*> o .: "age"

instance ToJSVal User where
  toJSVal (User {..}) = createObject [
      "name" .=> name
    , "age"  .=> age
    ]

instance FromJSVal User where
  fromJSVal o = runMaybeT $
    User <$> o .-> "name"
         <*> o .-> "age"
