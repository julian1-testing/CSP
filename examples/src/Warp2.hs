
{-# LANGUAGE OverloadedStrings #-}


-- IMPORTANT - how to wait/throttle according to the number of db connections - until one was available?

-- see here on routes, http://cjwebb.github.io/blog/2016/12/16/getting-started-with-haskells-warp/
-- more good doc (eg. middleware - gzip) http://www.yesodweb.com/book/web-application-interface   
-- https://crypto.stanford.edu/~blynn/haskell/warp.html

{-
  app :: Application
  app req res =
    res $ case rawPathInfo req of
      "/" -> helloRoute
      _   -> notFoundRoute
-}

module Warp2 where

import Network.Wai (responseLBS, Application, Response, rawPathInfo, requestMethod, remoteHost)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)

{-
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
-}

import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LT  -- 
import qualified Data.Text.Lazy.Encoding as LE


-- for putStrLn
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS

-- import qualified Data.ByteString.Char8 as BC


-- change to encodeBS
encode = LE.encodeUtf8 

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app


-- bytestring to putStrLn with bytestring 

printKeyVal key val =
  BS.putStrLn $ BS.append (BS.pack key) val


app :: Application
app req res = do


  -- see, https://hackage.haskell.org/package/wai-3.2.1.1/docs/Network-Wai.html
  let path = rawPathInfo req
  LBS.putStrLn $ encode "got request" 
  printKeyVal "path" path 

  printKeyVal "method " $ requestMethod req 
  printKeyVal "host " $ BS.pack $ show $ remoteHost req 


  res $ case path of
    "/whoot" -> whootRoute
    "/" -> helloRoute
    _   -> notFoundRoute


-- ok, now need to get parameters....
-- and url encoding/decoding...
-- actually we are directly matching this stuff... so perhaps we need a regex.... 
-- need to urlEncode / urlDecode 

-- no there's a difference between rawPathInfo and rawQueryString...


whootRoute :: Response
whootRoute =
  responseLBS status200 [(hContentType, "application/json")] . encode $ "Whoot"



helloRoute :: Response
helloRoute =
  responseLBS status200 [(hContentType, "application/json")] . encode $ "Hello World"


notFoundRoute :: Response
notFoundRoute =
    responseLBS status404 [(hContentType, "application/json")] "404 - Not Found"



{-

app :: Application
app req f = do
    print "got request"

    -- this works by itself, it's using Data.Text.Text
    let a = LT.pack "čušpajž日本語"  
    let b = LT.pack " whoot"  
    let c = LT.append a b

    -- 
    let d = LE.encodeUtf8 c

    print $ LBS.unpack d -- "sending data " -- kind of works.

    -- LBS.putStrLn  LBS.unpack d

    -- x <- f $ responseLBS status200 [(hContentType, "text/plain")] d --"Hello world!" 
    -- x <- f $ responseLBS status200 [(hContentType, "text/plain")] d --"Hello world!" 
    x <- f $ responseLBS status200 [(hContentType,  "text/html; charset=utf-8")] d --"Hello world!" 

    print "done request"
    return x
-}



