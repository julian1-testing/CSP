
{-# LANGUAGE Arrows , NoMonomorphismRestriction #-}

module Helpers where

-- TODO limit imports
import Text.XML.HXT.Core
-- import Text.XML.HXT.Core(runX, (>>>))


import Network.HTTP.Client(responseBody, responseStatus, managerResponseTimeout, RequestBody(RequestBodyBS), method, requestBody, requestHeaders, responseTimeoutMicro, parseRequest, newManager, httpLbs  )
import Network.HTTP.Client.TLS(tlsManagerSettings) 
import Network.HTTP.Types.Status(statusCode)

-- import Network.HTTP.Types.Method()
import Network.HTTP.Types.Header(HeaderName(..), hContentType)

-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString.Lazy.Char8 as BLC

import Data.Char(isSpace)




parseXML s = readString [ withValidate no
    , withRemoveWS yes  -- throw away formating WS
    ] s


atTag tag = deep (isElem >>> hasName tag)

atChildName s = getChildren >>> hasName s

getChildText = getChildren >>> getText

stripSpace = filter $ not.isSpace




doHTTPGet url = do
    let settings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro $ 60 * 1000000 }
    manager <- newManager settings
    request <- parseRequest url
    response <- httpLbs request manager
    -- Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    return response




doHTTPPost url body = do
    let settings = tlsManagerSettings  {
        managerResponseTimeout = responseTimeoutMicro $ 60 * 1000000
    }
    manager <- newManager settings
    -- get initial request
    initialRequest <- parseRequest url
    -- modify for post
    let request = initialRequest {
        method = BC.pack "POST",
        requestBody = RequestBodyBS $ BC.pack body,
        requestHeaders = [
            (hContentType, BC.pack "application/xml")
        ]
    }
    response <- httpLbs request manager
    Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    return response



