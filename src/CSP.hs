

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-
  CSP - catalog services for portal
  TODO - need to wait/throttle http connections to not db connections - until one was available?

  for routes,
    http://cjwebb.github.io/blog/2016/12/16/getting-started-with-haskells-warp/
  middleware - eg. gzip
    http://www.yesodweb.com/book/web-application-interface

  https://crypto.stanford.edu/~blynn/haskell/warp.html

  -- see vault - for storing data between apps and middleware.
-}


module CSP where


import Network.Wai
  (responseLBS, Application, Response, pathInfo, rawPathInfo, requestMethod,
    remoteHost, requestHeaders, queryString, rawQueryString )

import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType, hContentEncoding)


import qualified Data.Text.Encoding as E(encodeUtf8)
import qualified Data.Text.Lazy.Encoding as LE(encodeUtf8)
import qualified Data.Text.Lazy as LT

-- for putStrLn
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Database.PostgreSQL.Simple as PG(query, connectPostgreSQL)
import Database.PostgreSQL.Simple.Types as PG(Only(..))




import Search(request)

encode = LE.encodeUtf8


main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app


-- might be cleaner to format as a List
printKeyVal key val =
  BS.putStrLn $ BS.concat  [ (BS.pack key), E.encodeUtf8 " -> ", val ]



printReq req = do
  putStrLn "----"
  -- TODO tidy this crap....
  printKeyVal "path"          $ rawPathInfo req
  printKeyVal "rawQuery "     $ rawQueryString req
  printKeyVal "pathInfo "     $ (BS.pack.show) $ pathInfo req
  printKeyVal "method "       $ requestMethod req
  printKeyVal "host "         $ (BS.pack.show) $ remoteHost req
  printKeyVal "headers "      $ (BS.pack.show) $ requestHeaders req


printParams params = do
  -- log params
  putStrLn "----"
  putStrLn "params "
  -- putStrLn $ "length " ++ (show.length) params
  -- printKeyVal "params "       $ BS.pack $ show $ params
  let f (key, Just val) = BS.putStrLn $ BS.concat  [ key , E.encodeUtf8 " -> ", val ]
  mapM f params
  putStrLn "----"



app :: Application
app req res = do
  -- application routing

  -- see, https://hackage.haskell.org/package/wai-3.2.1.1/docs/Network-Wai.html
  -- LBS.putStrLn $ encode "got request"


  let params =  queryString req
  -- printReq req
  -- printParams params

  x <- case (pathInfo req) of

    [ "srv","eng","xml.search.imos" ] -> xmlSearchImos params

    [ "whoot" ] -> helloRoute

    _   -> notFoundRoute

  -- respond
  res x



-- xmlSearchImos :: IO Response
xmlSearchImos params =  do
  -- TODO only expose enough of the request to be able to handle it,
  -- printReq req
  printParams params

  BS.putStrLn $ E.encodeUtf8 "in xmlSearchImos"

  -- test db
  conn <- PG.connectPostgreSQL "host='postgres.localnet' dbname='harvest' user='harvest' sslmode='require'"
  let query = "select 123"
  xs :: [ (Only Integer ) ] <- PG.query conn query ()
  mapM (putStrLn.show) xs

  s <- Search.request conn
  return $
    -- application/xml;charset=UTF-8
    responseLBS status200 [
      (hContentType, "application/xml") ,
      (hContentEncoding, "UTF-8")
      ] . encode $  s



helloRoute :: IO Response
helloRoute = do
  LBS.putStrLn $ encode "in whoot hello"
  return $ responseLBS status200 [(hContentType, "application/json")] . encode $ "Hello World"



notFoundRoute :: IO Response
notFoundRoute =
  return $ responseLBS status404 [(hContentType, "application/json")] "404 - Not Found"


