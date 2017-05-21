{-
    general helper functions
-}

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

--
import qualified Data.ByteString.Char8 as BS(pack)
-- import qualified Data.ByteString.Lazy.Char8 as LBS 

import Data.Char(isSpace)

import qualified Data.Text.Lazy as LT(pack, empty, append)
import qualified Data.List as List(unfoldr)


-- XML helpers

parseXML s = readString [ withValidate no
    , withRemoveWS yes  -- throw away formating WS
    ] s

-- atTag does both deep and isElem and avoids children
atTag tag = deep (isElem >>> hasName tag)

atChildName s = getChildren >>> hasName s

getChildText = getChildren >>> getText

stripSpace = filter $ not.isSpace


-- Http helpers

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
        method = BS.pack "POST",
        requestBody = RequestBodyBS $ BS.pack body,
        requestHeaders = [
            (hContentType, BS.pack "application/xml")
        ]
    }
    response <- httpLbs request manager
    Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    return response



-- BS/string helpers

-- return white space String with length of count
-- TODO should probably use Bytestring or lazy text,  LT.pack LT.append etc
pad count =
  LT.pack $ List.unfoldr f count
  where 
    f 0 = Nothing
    f x = Just (' ', x - 1)


-- concat list of lazy text
concatLT lst = foldl LT.append LT.empty lst



