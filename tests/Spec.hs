{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Api
import           Control.Monad.Trans.Either
import           Test.Hspec

-- eitherAuditViews <- runEitherT $ apiGetAuditViews interface aq
-- var XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;
-- foreign import javascript unsafe "(function () { var auditTableHandlers = {}; auditTableHandlers.display = $1; return auditTableHandlers;}())"
--   initializeAuditTableHandlers :: AuditTableCallBack -> IO JSVal

-- sudo npm install -g xmlhttprequest
-- export NODE_PATH=/opt/lib/node_modules

-- expect package here
-- $HOME/node_modules/xmlhttprequest

-- module.exports.getTeam = getTeam;

-- foreign import javascript unsafe "var XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest; module.exports.XMLHttpRequest = XMLHttpRequest;"
--   importXMLHttpRequest :: IO ()

main :: IO ()
main = do

  -- importXMLHttpRequest
  interface <- createApiInterface
  -- eitherUser <- runEitherT $ apiGetUser interface (Just "James")
  eResult <- runEitherT $ apiPostUser interface (User "James" 40)
  case eResult of
    Left _ -> return ()
    Right r -> print r
  print "Hello from test"

  --fail "it failed"
