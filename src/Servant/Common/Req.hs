{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Servant.Common.Req where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.ByteString.Lazy hiding (pack, filter, map, null, elem, unpack)
import           Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString as BS
import           Data.CaseInsensitive
import           Data.Char
import           Data.IORef
import           Data.JSString (JSString)
import qualified Data.JSString as JSString
import           Data.String
import           Data.String.Conversions
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List.Split
import           Data.Maybe
import           Data.Text.Encoding
import           Data.Typeable
import           Data.Primitive.Addr
import           Data.Primitive.ByteArray
import           Data.ByteString.Unsafe (unsafePackAddressLen)
import           GHCJS.Foreign (jsTrue, jsFalse)
import           GHCJS.Foreign.Callback ( Callback (..)
                                        , OnBlocked(..)
                                        , syncCallback)
import           GHCJS.Foreign.QQ
import           GHCJS.Marshal
import           GHCJS.Prim
import           Network.HTTP.Media hiding (Accept)
import           Network.HTTP.Types
import qualified Network.HTTP.Types.Header   as HTTP
import           Network.URI
import           Servant.API.ContentTypes
import           Servant.Common.BaseUrl

import           System.IO.Unsafe
import           Unsafe.Coerce
import           Web.HttpApiData



data ServantError
  = FailureResponse
    { responseStatus            :: Status
    , responseContentType       :: MediaType
    , responseBody              :: JSVal
    }
  | DecodeFailure
    { decodeError               :: String
    , responseContentType       :: MediaType
    , responseBody              :: JSVal
    }
  | UnsupportedContentType
    { responseContentType       :: MediaType
    , responseBody              :: JSVal
    }
  | InvalidContentTypeHeader
    { responseContentTypeHeader :: ByteString
    , responseBody              :: JSVal
    }
  deriving (Typeable)


-- there is no show instance because fromJSVal is an IO function
printServantError :: ServantError -> IO ()
printServantError (FailureResponse x y z) = do
  print "FailureResponse"
  print x
  print y
  pz <- (fromJSVal z :: IO (Maybe JSString))
  print pz
printServantError (DecodeFailure x y z)   = do
  print "DecodeFailure"
  print x
  print y
  pz <- (fromJSVal z :: IO (Maybe JSString))
  print pz
printServantError (UnsupportedContentType x y) = do
  print "UnsupportedContentType"
  print x
  py <- (fromJSVal y :: IO (Maybe JSString))
  print py
printServantError (InvalidContentTypeHeader x y) = do
  print "InvalidContentTypeHeader"
  print x
  py <- (fromJSVal y :: IO (Maybe JSString))
  print py

data ForeignRetention
  = NeverRetain                   -- ^ do not retain data unless the callback is directly
                                  --   referenced by a Haskell thread.
  | AlwaysRetain                  -- ^ retain references indefinitely, until `freeCallback`
                                  --   is called (the callback will be kept in memory until it's freed)
  | DomRetain JSVal               -- ^ retain data as long as the `JSVal` is a DOM element in
                                  --   `window.document` or in a DOM tree referenced by a Haskell
                                  --    thread.

data Req = Req
  { reqPath   :: String
  , qs        :: QueryText
  , reqBody   :: Maybe (IO JSVal, MediaType)
  , reqAccept :: [MediaType]
  , headers   :: [(String, Text)]
  }

defReq :: Req
defReq = Req "" [] Nothing [] []

appendToPath :: String -> Req -> Req
appendToPath p req =
  req { reqPath = reqPath req ++ "/" ++ p }

appendToMatrixParams :: String
                     -> Maybe String
                     -> Req
                     -> Req
appendToMatrixParams pname pvalue req =
  req { reqPath = reqPath req ++ ";" ++ pname ++ maybe "" ("=" ++) pvalue }

appendToQueryString :: Text       -- ^ param name
                    -> Maybe Text -- ^ param value
                    -> Req
                    -> Req
appendToQueryString pname pvalue req =
  req { qs = qs req ++ [(pname, pvalue)]
      }

{-
addHeader :: ToText a => String -> a -> Req -> Req
addHeader name val req = req { headers = headers req
                                      ++ [(name, toText val)]
                             }
-}
addHeader :: ToHttpApiData a => String -> a -> Req -> Req
addHeader name val req = req { headers = headers req
                                      ++ [(name, toQueryParam val)]
                             }


setRQBody :: IO JSVal -> MediaType -> Req -> Req
setRQBody b t req = req { reqBody = Just (b, t) }

displayHttpRequest :: Method -> String
displayHttpRequest httpmethod = "HTTP " ++ cs httpmethod ++ " request"

performRequest :: Method -> Req -> (Int -> Bool) -> Maybe BaseUrl
               -> EitherT ServantError IO ( Int, JSVal, MediaType
                                          , [HTTP.Header])
performRequest reqMethod req isWantedStatus reqHost = do
  eResp <- liftIO $ makeRequest reqMethod req isWantedStatus reqHost
  case eResp of
    (Left err) -> left err
    (Right (status_code, hrds, body)) -> do
      ct <- case lookup "Content-Type" hrds of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> left $ InvalidContentTypeHeader (cs t) body
                   Just t' -> pure t'
      return (status_code, body, ct, hrds)


performRequestCT :: GHCJSUnrender ct result =>
  Proxy ct -> Method -> Req -> [Int] -> Maybe BaseUrl -> EitherT ServantError IO ([HTTP.Header], result)
performRequestCT ct reqMethod req wantedStatus reqHost = do
  let acceptCT = contentType ct
  (_status, respBody, respCT, hrds) <-
    performRequest reqMethod (req { reqAccept = [acceptCT] }) (`elem` wantedStatus) reqHost
  unless (matches respCT (acceptCT)) $ left $ UnsupportedContentType respCT respBody
  res <- liftIO $ ghcjsUnrender ct respBody
  case res of
    Left err -> left $ DecodeFailure err respCT respBody
    Right val -> return (hrds, val)

performRequestNoBody :: Method -> Req -> [Int] -> Maybe BaseUrl -> EitherT ServantError IO ()
performRequestNoBody reqMethod req wantedStatus reqHost = do
  _ <- performRequest reqMethod req (`elem` wantedStatus) reqHost
  return ()



-- foreign import javascript unsafe "var XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest; new XMLHttpRequest();"
-- tests are performed with node. it doesnt natively hav XMLHttpRequest
-- this makes this function useable in node or javascript
foreign import javascript unsafe "(function () {if (typeof XMLHttpRequest === 'undefined') { XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest; return new XMLHttpRequest(); } else { return new XMLHttpRequest(); } }())"
  jsXhrRequest :: IO JSVal
foreign import javascript unsafe "$1.open($2, $3, $4)"
  jsXhrOpen :: JSVal -> JSString -> JSString -> JSVal -> IO ()
foreign import javascript unsafe "$1.send()"
  jsXhrSend :: JSVal -> IO ()
foreign import javascript unsafe "$1.send($2)"
  jsXhrSendWith :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "$1.onreadystatechange = $2"
  jsXhrOnReadyStateChange:: JSVal -> Callback (IO ()) -> IO ()
foreign import javascript unsafe "$1.readyState"
  jsXhrReadyState:: JSVal -> IO JSVal
foreign import javascript unsafe "$1.responseText"
  jsXhrResponseText:: JSVal -> IO JSString
--foreign import javascript unsafe "$1.response"
jsXhrResponse:: JSVal -> IO JSVal
jsXhrResponse jsv = [jsu|
(function () {
   var contentResponse = typeof `jsv.response;
   if( contentResponse == "undefined" ) { // This takes care of the lack of a 'response' field in ie9
    return JSON.parse(`jsv.responseText);
   } else if (contentResponse == "string" ) { // IE11 bug
    return JSON.parse(`jsv.response);
   } else {
    return `jsv.response;
   }
}())
|]

foreign import javascript unsafe "$1.responseType = $2"
  jsXhrResponseType:: JSVal -> JSString -> IO ()
foreign import javascript unsafe "$1.status"
  jsXhrStatus:: JSVal -> IO JSVal
foreign import javascript unsafe "$1.getAllResponseHeaders()"
  jsXhrResponseHeaders :: JSVal -> IO JSString
foreign import javascript unsafe "$1.setRequestHeader($2, $3)"
  jsXhrSetRequestHeader :: JSVal -> JSString -> JSString -> IO ()
foreign import javascript unsafe "$1.statusText"
  jsXhrGetStatusText :: JSVal -> IO JSString
foreign import javascript unsafe "xh = $1"
  jsDebugXhr :: JSVal -> IO ()
foreign import javascript safe "h$wrapBuffer($3, true, $1, $2)"
  js_wrapBuffer :: Int -> Int -> JSVal -> IO JSVal
foreign import javascript unsafe "h$release($1)"
  js_release :: Callback (IO ()) -> IO ()
foreign import javascript unsafe "JSON.stringify($1)"
  js_stringify :: JSVal -> IO JSVal

xhrResponseHeaders :: JSVal -> IO [HTTP.Header]
xhrResponseHeaders jReq = do
  (headers :: JSString) <- jsXhrResponseHeaders jReq
  let headersStrings = T.lines . T.pack . JSString.unpack $ headers
  return $ catMaybes $ buildHeader <$> headersStrings


buildHeader :: Text -> Maybe HTTP.Header
buildHeader xs = parseXs $ splitStr xs
  where splitStr = T.splitOn (":")
        parseXs :: [Text] -> Maybe HTTP.Header
        parseXs (c:cs) = Just (mk $ encodeUtf8 $ T.strip c, encodeUtf8 $ T.strip $ T.concat cs)
        parseXs _ = Nothing

bufferByteString :: Int        -- ^ offset from the start in bytes
                 -> Int        -- ^ length in bytes (use zero or a negative number to get the whole ArrayBuffer)
                 -> JSVal
                 -> IO BS.ByteString
bufferByteString offset length buf = do
  (ByteArray ba) <- wrapBuffer offset length buf
  byteArrayByteString ba

byteArrayByteString :: ByteArray# -> IO BS.ByteString
byteArrayByteString arr =
#ifdef ghcjs_HOST_OS
  let ba        = ByteArray arr
      !(Addr a) = byteArrayContents ba
  in  unsafePackAddressLen (sizeofByteArray ba) a
#else
  error "GHCJS.Foreign.byteArrayToByteString: not JS"
#endif

wrapBuffer :: Int          -- ^ offset from the start in bytes, if this is not a multiple of 8,
                           --   not all types can be read from the ByteArray#
           -> Int          -- ^ length in bytes (use zero or a negative number to use the whole ArrayBuffer)
           -> JSVal        -- ^ JavaScript ArrayBuffer object
           -> IO ByteArray -- ^ result
wrapBuffer offset size buf = unsafeCoerce <$> js_wrapBuffer offset size buf
{-# INLINE wrapBuffer #-}

makeRequest :: Method -> Req -> (Int -> Bool) -> Maybe BaseUrl -> IO (Either ServantError (Int, [HTTP.Header], JSVal))
makeRequest method req isWantedStatus bUrl = do
  jRequest <- jsXhrRequest
  let url = JSString.pack . show  $ buildUrl req bUrl
      methodText = JSString.pack $ unpack method
  jsXhrOpen jRequest methodText url jsTrue
  jsXhrResponseType jRequest "json"
  resp <- newEmptyMVar
  cb <- syncCallback ThrowWouldBlock $ do
    r <- jsXhrReadyState jRequest :: IO JSVal
    state <- fromJSVal r
    when ((state :: Maybe Int) == Just 4) $ do
      statusCode <- fromMaybe (-1) <$> (fromJSVal =<< jsXhrStatus jRequest)
      if (statusCode >= 200 && statusCode < 300)
        then do
          bsResp <- jsXhrResponse jRequest
          headers <- xhrResponseHeaders jRequest
          putMVar resp $ Right (statusCode, headers, bsResp)
        else do
          bsStatusText <- jsXhrGetStatusText jRequest
          respBody <- jsXhrResponse jRequest
          [js_| console.log(`respBody); |]
          putMVar resp $ Left $ FailureResponse (mkStatus statusCode .
                                                       pack . JSString.unpack $ bsStatusText)
                                                ("unknown" // "unknown")
                                                (respBody)


  jsXhrOnReadyStateChange jRequest cb
  case reqBody req of
    Nothing -> jsXhrSend jRequest
    (Just (body, mediaType)) -> do
      jsXhrSetRequestHeader jRequest "Content-Type" $ JSString.pack $ show mediaType
      jsXhrSendWith jRequest =<< js_stringify =<< body
  res <- takeMVar resp
  release cb
  return res

release :: Callback (IO ()) -- ^ the callback
                 -> IO ()
release = js_release

buildUrl :: Req -> Maybe BaseUrl -> URI
buildUrl req@(Req path qText mBody rAccept hs) baseurl =
  (baseURI baseurl){uriPath = path, uriQuery = query}
  where
    query = unpack $ renderQuery True $ queryTextToQuery qText
    baseURI Nothing = nullURI
    baseURI (Just (BaseUrl scheme host port)) =
      nullURI {
        uriScheme = schemeText,
        uriAuthority = Just $ URIAuth "" host portText
      }
      where
        portText = ":" <> (show port)
        schemeText = case scheme of
                            Http -> "http:"
                            Https -> "https:"
class Accept ctype => GHCJSUnrender ctype a where
  ghcjsUnrender :: Proxy ctype -> JSVal -> IO (Either String a)

instance FromJSVal a => GHCJSUnrender JSON a where
  ghcjsUnrender _ val = maybe (Left "Error when marshalling from JSVal") Right  <$> fromJSVal val
