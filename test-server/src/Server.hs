module Server where

import Api
import Control.Monad.IO.Class  (liftIO)
import Network.Wai
import Servant

app :: Application
app = serve serverApi $ server

serverApi :: Proxy Api
serverApi = Proxy

server :: Server Api
server = getUserH :<|> postUserH :<|> deleteUserH :<|> existsUserH :<|> upsertUserH
  where
    getUserH  mUserName = do
      liftIO $ print "getUser"
      return (User <$> mUserName <*> pure 25)
    postUserH user      = do
      liftIO $ print "postUser"
      return . Just $ user
    deleteUserH user      = do
      liftIO $ print "deleteUser"
      return True
    existsUserH user      = do
      liftIO $ print "existsUser"
      return True
    upsertUserH user      = do
      liftIO $ print "upsertUser"
      return user
