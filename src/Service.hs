{-
  CSP Service - catalog services for portal

  main webapp and routing

  TODO - maybe change the name of this to Router? or Service or Web ?
  TODO - need to wait/throttle http connections to not db connections - until one was available?

  for routes,
    http://cjwebb.github.io/blog/2016/12/16/getting-started-with-haskells-warp/
  middleware - eg. gzip
    http://www.yesodweb.com/book/web-application-interface

  https://crypto.stanford.edu/~blynn/haskell/warp.html

  -- see vault - for storing data between apps and middleware.

  http://localhost:3000/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability
-}



{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}


module Service where


import Network.Wai
  (responseLBS, Application, Response, pathInfo, rawPathInfo, requestMethod,
    remoteHost, requestHeaders, queryString, rawQueryString )

import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType, hContentEncoding)


import qualified Data.Text.Encoding as E(encodeUtf8)
import qualified Data.Text.Lazy.Encoding as LE(encodeUtf8)
-- import qualified Data.Text.Lazy as LT

import qualified Data.ByteString.Char8 as BS(putStrLn, pack, concat, readInt)
import qualified Data.ByteString.Lazy.Char8 as LBS(readFile)


import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import qualified Database.PostgreSQL.Simple.Types as PG(Only(..))

import qualified Data.List as L(find)

import qualified Network.HTTP.Types as HTTP(urlEncode, urlDecode)


import Search(search, Params(..))


encode = LE.encodeUtf8


main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app




printReq req = do
  putStrLn "----"
  BS.putStrLn. BS.concat $ [
    "\npath: ",     rawPathInfo req,
    "\nrawQuery: ", rawQueryString req,
    "\npathInfo: ", (BS.pack.show) $ pathInfo req,
    "\nmethod: ",   requestMethod req,
    "\nhost: ",     (BS.pack.show) $ remoteHost req,
    "\nheaders: ",  (BS.pack.show) $ requestHeaders req
    ]


printParams params = do
  putStrLn "----"
  putStrLn "params "
  -- putStrLn $ "length " ++ (show.length) params
  -- printKeyVal "params "       $ BS.pack $ show $ params
  mapM (BS.putStrLn. BS.concat .f) params
  where
    f (key, Just val) = [ key , ": ", val ]
    f (key, _ ) = [ key , ": _ " ]


-- important - we should probably be decoding and validating params before dispatching...


app :: Application
app req res = do
  -- application routing
  -- see, https://hackage.haskell.org/package/wai-3.2.1.1/docs/Network-Wai.html

  let params =  queryString req
  -- LBS.putStrLn $ encode "got request"
  -- printReq req
  -- printParams params

  -- TODO check if this works for POST and GET?
  x <- case (pathInfo req) of

    [ "srv","eng","xml.search.imos" ] -> do
      printReq req
      xmlSearchImos params

    [ "images", "logos", imageId ] -> imagesLogos imageId

    [ "hello" ] -> hello

    _ -> notFound

  res x




extractIntParam params key = do
  -- over an option monad - find, the tuple val, and readInt are all option types
  (_, v_) <- L.find f params
  v <- v_
  (i, _) <- BS.readInt v
  return i
  where
    f (k, _) = k == key


extractStringParam params key = do
  -- over an option monad
  (_, v_) <- L.find f params
  v <- v_
  -- it's not clear urlDecode is needed for 'facet.q' but not for 'protocol'

  -- treat exmplit empty strings eg. any="" as Nothing values also 
  case HTTP.urlDecode False v of 
    "" -> Nothing
    x -> return x

  -- return $ HTTP.urlDecode False v
  where
    f (k, _) = k == key




-- xmlSearchImos :: IO Response
xmlSearchImos params = do
  -- get a db connection, extract the params and delegate off to search
  BS.putStrLn $ E.encodeUtf8 "xmlSearchImos"
  printParams params

  -- TODO fix this destructuring is no good
  let Just from = extractIntParam params "from"     -- pagination
  let Just to = extractIntParam params "to"
  let facetQ = extractStringParam params "facet.q"  -- facet search expression
  let any = extractStringParam params "any"         -- freetext

  -- any: argo profile
  -- facet.q: Platform/Vessel/vessel%20of%20opportunity
  -- HTTP.urlDecode False v

  let searchParams = Search.Params {
      to = to,
      from = from,
      facetQ = facetQ,
      Search.any = any
  }

  -- test db
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  s <- Search.search conn searchParams

  return $
    -- application/xml;charset=UTF-8
    responseLBS status200 [
      (hContentType, "application/xml") ,
      (hContentEncoding, "UTF-8")
      ] . encode $ s



imagesLogos imageId = do
  -- BS.putStrLn $ E.encodeUtf8 imageId
  s <- LBS.readFile "resources/logo.png"
  return $
    responseLBS status200 [ (hContentType, "image/png") ] s



hello :: IO Response
hello = do
  BS.putStrLn "got hello"
  return $ responseLBS status200 [(hContentType, "application/json")] . encode $ "Hello World!"



notFound :: IO Response
notFound =
  return $ responseLBS status404 [(hContentType, "application/json")] "404 - Not Found"



{-
  simple db query
  let query = "select 123"
  xs :: [ (Only Int ) ] <- PG.query conn query ()
  mapM (putStrLn.show) xs
-}


