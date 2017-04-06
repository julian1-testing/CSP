{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)


import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E



main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app


-- how would we throttle this according to the number of db connections - until one was available?
-- 

app :: Application
app req f = do
    print "got request"

    -- this works by itself, it's using Data.Text.Text
    let a = T.pack "čušpajž日本語"  
    let b = T.pack " whoot日本語"  
    let c = T.append a b

    -- 
    let d = E.encodeUtf8 c


    -- responseLBS - expects a Lazy Byte String - while we have

    -- x <- f $ responseLBS status200 [(hContentType, "text/plain")] d --"Hello world!" 
    x <- f $ responseLBS status200 [(hContentType, "text/plain")] d --"Hello world!" 

    print "done request"
    return x

