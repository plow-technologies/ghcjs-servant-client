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

foreign import javascript unsafe "var XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest;"
  importXMLHttpRequest :: IO ()

main :: IO ()
main = do
  importXMLHttpRequest
  interface <- createApiInterface
  eitherUser <- runEitherT $ apiGetUser interface (Just "James")
  print "Hello from test"
  fail "it failed"
