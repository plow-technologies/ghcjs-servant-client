{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Common.Req where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.ByteString.Lazy hiding (pack, filter, map, null, elem, unpack)
import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString as BS
import Data.IORef
import Data.String
import Data.String.Conversions
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Typeable
import Network.HTTP.Media
import Network.HTTP.Types
import qualified Network.HTTP.Types.Header   as HTTP
import Network.URI
import Servant.API.ContentTypes
import Servant.Common.BaseUrl
import Servant.Common.Text
import System.IO.Unsafe
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal
import Control.Concurrent.MVar
import Data.List.Split
import Data.Maybe
import Data.CaseInsensitive
import Data.Char

data ServantError
  = FailureResponse
    { responseStatus            :: Status
    , responseContentType       :: MediaType
    , responseBody              :: ByteString
    }
  | DecodeFailure
    { decodeError               :: String
    , responseContentType       :: MediaType
    , responseBody              :: ByteString
    }
  | UnsupportedContentType
    { responseContentType       :: MediaType
    , responseBody              :: ByteString
    }
  | InvalidContentTypeHeader
    { responseContentTypeHeader :: ByteString
    , responseBody              :: ByteString
    }
  deriving (Show, Typeable)

instance Exception ServantError

data Req = Req
  { reqPath   :: String
  , qs        :: QueryText
  , reqBody   :: Maybe (ByteString, MediaType)
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

addHeader :: ToText a => String -> a -> Req -> Req
addHeader name val req = req { headers = headers req
                                      ++ [(name, toText val)]
                             }

setRQBody :: ByteString -> MediaType -> Req -> Req
setRQBody b t req = req { reqBody = Just (b, t) }

displayHttpRequest :: Method -> String
displayHttpRequest httpmethod = "HTTP " ++ cs httpmethod ++ " request"

performRequest :: Method -> Req -> (Int -> Bool) -> BaseUrl
               -> EitherT ServantError IO ( Int, ByteString, MediaType
                                          , [HTTP.Header])
performRequest reqMethod req isWantedStatus reqHost = do
  (status_code, hrds, body) <- liftIO $ makeRequest reqMethod req isWantedStatus reqHost
  ct <- case lookup "Content-Type" hrds of
             Nothing -> pure $ "application"//"octet-stream"
             Just t -> case parseAccept t of
               Nothing -> left $ InvalidContentTypeHeader (cs t) $ fromStrict body
               Just t' -> pure t'
  return (status_code, fromStrict body, ct, hrds)


performRequestCT :: MimeUnrender ct result =>
  Proxy ct -> Method -> Req -> [Int] -> BaseUrl -> EitherT ServantError IO ([HTTP.Header], result)
performRequestCT ct reqMethod req wantedStatus reqHost = do
  let acceptCT = contentType ct
  (_status, respBody, respCT, hrds) <-
    performRequest reqMethod (req { reqAccept = [acceptCT] }) (`elem` wantedStatus) reqHost
  unless (matches respCT (acceptCT)) $ left $ UnsupportedContentType respCT respBody
  case mimeUnrender ct respBody of
    Left err -> left $ DecodeFailure err respCT respBody
    Right val -> return (hrds, val)

performRequestNoBody :: Method -> Req -> [Int] -> BaseUrl -> EitherT ServantError IO ()
performRequestNoBody reqMethod req wantedStatus reqHost = do
  _ <- performRequest reqMethod req (`elem` wantedStatus) reqHost
  return ()


data XMLHttpRequest

foreign import javascript unsafe "new XMLHttpRequest()" 
  jsXhrRequest :: IO (JSRef XMLHttpRequest)
foreign import javascript unsafe "$1.open($2, $3, $4)" 
  jsXhrOpen :: JSRef XMLHttpRequest -> JSString -> JSString -> JSBool -> IO ()
foreign import javascript unsafe "$1.send()" 
  jsXhrSend :: JSRef XMLHttpRequest ->  IO ()
foreign import javascript unsafe "$1.send($2)"
  jsXhrSendWith :: JSRef XMLHttpRequest -> JSRef a -> IO ()
foreign import javascript unsafe "$1.onreadystatechange = $2"  
  jsXhrOnReadyStateChange:: JSRef XMLHttpRequest -> JSFun (IO ()) -> IO ()
foreign import javascript unsafe "$1.readyState"  
  jsXhrReadyState:: JSRef XMLHttpRequest -> IO (JSRef Int)
foreign import javascript unsafe "$1.responseText"  
  jsXhrResponseText:: JSRef XMLHttpRequest -> IO JSString
foreign import javascript unsafe "$1.response"  
  jsXhrResponse:: JSRef XMLHttpRequest -> IO (JSRef a)
foreign import javascript unsafe "$1.responseType = $2"  
  jsXhrResponseType:: JSRef XMLHttpRequest -> JSString -> IO ()
foreign import javascript unsafe "$1.status"  
  jsXhrStatus:: JSRef XMLHttpRequest -> IO (JSRef Int)
foreign import javascript unsafe "$1.getAllResponseHeaders()"
  jsXhrResponseHeaders :: JSRef XMLHttpRequest -> IO JSString
foreign import javascript unsafe "$1.setRequestHeader($2, $3)"
  jsXhrSetRequestHeader :: JSRef XMLHttpRequest -> JSString -> JSString -> IO ()
foreign import javascript unsafe "xh = $1"
  jsDebugXhr :: JSRef XMLHttpRequest -> IO ()
foreign import javascript unsafe "console.log($1)"
  jsDebugJSRef :: JSRef a -> IO ()


xhrResponseHeaders :: JSRef XMLHttpRequest -> IO [HTTP.Header]
xhrResponseHeaders jReq = do
  headersStrings <-  T.lines . fromJSString <$> jsXhrResponseHeaders jReq
  return $ catMaybes $ buildHeader <$> headersStrings


buildHeader :: Text -> Maybe HTTP.Header
buildHeader xs = parseXs $ splitStr xs
  where splitStr = T.splitOn (":")
        parseXs :: [Text] -> Maybe HTTP.Header
        parseXs (c:cs) = Just (mk $ encodeUtf8 $ T.strip c, encodeUtf8 $ T.strip $ T.concat cs)
        parseXs _ = Nothing


makeRequest :: Method -> Req -> (Int -> Bool) -> BaseUrl -> IO (Int, [HTTP.Header], BS.ByteString)
makeRequest method req isWantedStatus bUrl = do
  jRequest <- jsXhrRequest
  let url = toJSString . show $ buildUrl req bUrl
      methodText = toJSString $ unpack method
  jsXhrOpen jRequest methodText url jsTrue
  jsXhrResponseType jRequest "arraybuffer"
  resp <- newEmptyMVar
  cb <- syncCallback AlwaysRetain True $ do
    state <- fromJSRef =<< jsXhrReadyState jRequest
    when (state == Just 4) $ do
      statusCode <- fromJSRef =<< jsXhrStatus jRequest
      when (statusCode >= Just 200 && statusCode < Just 300) $ do
        bsResp <- bufferByteString 0 0 =<< jsXhrResponse jRequest
        headers <- xhrResponseHeaders jRequest
        putMVar resp (fromMaybe (-1) statusCode, headers, bsResp)
  jsXhrOnReadyStateChange jRequest cb
  case reqBody req of
    Nothing -> jsXhrSend jRequest
    (Just (body, mediaType)) -> do
      jsXhrSetRequestHeader jRequest "Content-Type" $ toJSString $ show mediaType
      b <- toJSRef (decodeUtf8 $ toStrict body)
      jsDebugJSRef b
      jsXhrSendWith jRequest b
  jsDebugXhr jRequest
  res <- takeMVar resp
  release cb
  return res

buildUrl :: Req -> BaseUrl -> URI
buildUrl (Req path qText mBody rAccept hs) (BaseUrl scheme host port) = 
  nullURI {
    uriScheme = schemeText,
    uriAuthority = Just $ URIAuth "" host portText,
    uriPath = path
  }
  where schemeText = case scheme of
                      Http -> "http:"
                      Https -> "https:"
        portText = ":" <> (show port)