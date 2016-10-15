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
server = getUserH :<|> postUserH
  where
    -- getUserH :: String -> EitherT ServantErr IO User
    getUserH  mUserName = return (User <$> mUserName <*> pure 25)
    postUserH user     = return ()
